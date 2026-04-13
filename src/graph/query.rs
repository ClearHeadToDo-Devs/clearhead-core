//! SPARQL query and graph reconstruction utilities.

use super::{
    actions_pred, bfo_pred, cco_node, rdfs_pred, GraphError, Result, Store, ACTIONS_NS, BFO_HAS_PART,
    BFO_NS, BFO_PART_OF, CCO_IS_SUCCESSOR_OF, CCO_NS, CCO_PLAN, CCO_PLANNED_ACT, CCO_PRESCRIBED_BY,
    CCO_PRESCRIBES, CCO_STATUS_PROP, RDFS_COMMENT, RDFS_LABEL,
};
use crate::domain::{ActPhase, Charter, DomainModel, Plan, PlannedAct, Recurrence};
use chrono::{DateTime, Local};
use oxigraph::model::{GraphName, NamedNode, NamedOrBlankNode, Term};
use oxigraph::sparql::{QueryResults, SparqlEvaluator};
use std::collections::HashMap;
use uuid::Uuid;

type Row = HashMap<String, String>;

pub fn query_action_ids(store: &Store, sparql: &str) -> Result<Vec<String>> {
    query_ids(store, sparql, "id")
}

pub fn query_raw(store: &Store, sparql: &str) -> Result<Vec<Row>> {
    execute_select_rows(store, sparql)
}

pub fn build_raw_where_query(where_clause: &str) -> String {
    format!(
        "PREFIX actions: <{ACTIONS_NS}>\nPREFIX cco: <{CCO_NS}>\n\
         PREFIX bfo: <{bfo}>\nPREFIX rdfs: <{rdfs}>\nPREFIX rdf: <{rdf}>\n\
         PREFIX xsd: <{xsd}>\nPREFIX skos: <{skos}>\n\
         SELECT * WHERE {{ {where_clause} }}",
        bfo = BFO_NS,
        rdfs = "http://www.w3.org/2000/01/rdf-schema#",
        rdf = super::RDF_NS,
        xsd = super::XSD_NS,
        skos = super::SKOS_NS,
    )
}

pub fn build_where_query(where_clause: &str, _select: Option<&str>, _from: Option<&str>) -> String {
    format!(
        "PREFIX actions: <{actions_ns}>\n\
         PREFIX cco: <{cco_ns}>\n\
         PREFIX bfo: <{bfo_ns}>\n\
         PREFIX rdfs: <{rdfs_ns}>\n\
         PREFIX rdf: <{rdf_ns}>\n\
         PREFIX xsd: <{xsd_ns}>\n\
         PREFIX skos: <{skos_ns}>\n\
         SELECT ?id WHERE {{ ?s <{actions_ns}hasUUID> ?id . {{ {where_clause} }} }}",
        actions_ns = ACTIONS_NS,
        cco_ns = CCO_NS,
        bfo_ns = BFO_NS,
        rdfs_ns = "http://www.w3.org/2000/01/rdf-schema#",
        rdf_ns = super::RDF_NS,
        xsd_ns = super::XSD_NS,
        skos_ns = super::SKOS_NS,
    )
}

pub fn load_domain_model_from_store(store: &Store) -> Result<DomainModel> {
    let charters = query_charters(store)?;
    let plans = query_plans(store)?;
    let acts = query_acts(store)?;

    let mut acts_by_plan: HashMap<Uuid, Vec<PlannedAct>> = HashMap::new();
    for act in acts {
        acts_by_plan.entry(act.plan_id).or_default().push(act);
    }

    let mut plans_by_id: HashMap<Uuid, Plan> = plans
        .into_iter()
        .map(|mut plan| {
            if let Some(mut plan_acts) = acts_by_plan.remove(&plan.id) {
                plan_acts.sort_by_key(|a| a.id);
                plan.acts = plan_acts;
            }
            (plan.id, plan)
        })
        .collect();

    let mut plans_by_charter: HashMap<Uuid, Vec<Plan>> = HashMap::new();
    for (plan_id, charter_id) in query_charter_plan_edges(store)? {
        if let Some(plan) = plans_by_id.remove(&plan_id) {
            plans_by_charter.entry(charter_id).or_default().push(plan);
        }
    }

    let mut final_charters: Vec<Charter> = charters
        .into_iter()
        .map(|mut charter| {
            let mut charter_plans = plans_by_charter.remove(&charter.id).unwrap_or_default();
            charter_plans.sort_by_key(|p| p.id);
            charter.plans = charter_plans;
            charter
        })
        .collect();
    final_charters.sort_by_key(|c| c.id);

    Ok(DomainModel {
        objectives: vec![],
        charters: final_charters,
    })
}

pub fn load_planned_acts_from_store(store: &Store) -> Result<Vec<PlannedAct>> {
    query_acts(store)
}

pub fn validate_actions_vocabulary(store: &Store) -> Result<Vec<String>> {
    let mut violations = Vec::new();

    let q_missing_status = format!(
        "SELECT ?act WHERE {{ \
            ?act a <{cco}{planned_act}> . \
            FILTER NOT EXISTS {{ ?act <{cco}{status_prop}> ?s }} \
        }}",
        cco = CCO_NS,
        planned_act = CCO_PLANNED_ACT,
        status_prop = CCO_STATUS_PROP,
    );
    for uri in query_term_values(store, &q_missing_status, "act")? {
        violations.push(format!(
            "PlannedActStatusShape: <{uri}> is missing a status (cco:{prop})",
            prop = CCO_STATUS_PROP,
        ));
    }

    let q_invalid_status = format!(
        "SELECT ?act WHERE {{ \
            ?act <{cco}{status_prop}> ?s . \
            FILTER (?s NOT IN ( \
                <{ns}NotStarted>, \
                <{ns}InProgress>, \
                <{ns}Completed>, \
                <{ns}Blocked>, \
                <{ns}Cancelled> \
            )) \
        }}",
        cco = CCO_NS,
        status_prop = CCO_STATUS_PROP,
        ns = ACTIONS_NS,
    );
    for uri in query_term_values(store, &q_invalid_status, "act")? {
        violations.push(format!(
            "PlannedActStatusShape (sh:in): <{uri}> has an unrecognized status value",
        ));
    }

    let q_prescribes_wrong_target = format!(
        "SELECT ?plan WHERE {{ \
            ?plan a <{cco}{plan_cls}> . \
            ?plan <{cco}{prescribes}> ?target . \
            FILTER NOT EXISTS {{ ?target a <{cco}{planned_act}> }} \
        }}",
        cco = CCO_NS,
        plan_cls = CCO_PLAN,
        prescribes = CCO_PRESCRIBES,
        planned_act = CCO_PLANNED_ACT,
    );
    for uri in query_term_values(store, &q_prescribes_wrong_target, "plan")? {
        violations.push(format!(
            "PlanPrescribesShape: <{uri}> has a prescribes target that is not a PlannedAct",
        ));
    }

    Ok(violations)
}

fn query_charters(store: &Store) -> Result<Vec<Charter>> {
    let sparql = format!(
        "SELECT ?id WHERE {{ ?s a <{actions}Charter> . ?s <{actions}hasUUID> ?id . }}",
        actions = ACTIONS_NS,
    );
    let ids = query_uuids(store, &sparql, "id", "charter")?;
    ids.into_iter()
        .map(|id| get_charter_by_id(store, id))
        .collect()
}

fn query_plans(store: &Store) -> Result<Vec<Plan>> {
    let sparql = format!(
        "SELECT ?id WHERE {{ ?s a <{cco}{plan}> . ?s <{actions}hasUUID> ?id . }}",
        cco = CCO_NS,
        plan = CCO_PLAN,
        actions = ACTIONS_NS,
    );
    let ids = query_uuids(store, &sparql, "id", "plan")?;
    ids.into_iter()
        .map(|id| get_plan_by_id(store, id))
        .collect()
}

fn query_acts(store: &Store) -> Result<Vec<PlannedAct>> {
    let sparql = format!(
        "SELECT ?id WHERE {{ ?s a <{cco}{planned_act}> . ?s <{actions}hasUUID> ?id . }}",
        cco = CCO_NS,
        planned_act = CCO_PLANNED_ACT,
        actions = ACTIONS_NS,
    );
    let ids = query_uuids(store, &sparql, "id", "act")?;
    ids.into_iter()
        .map(|id| get_planned_act_by_id(store, id))
        .collect()
}

fn query_charter_plan_edges(store: &Store) -> Result<Vec<(Uuid, Uuid)>> {
    let sparql = format!(
        "SELECT ?charterId ?planId WHERE {{ \
            ?charter a <{actions}Charter> ; <{actions}hasUUID> ?charterId ; <{bfo}{has_part}> ?plan . \
            ?plan a <{cco}{plan}> ; <{actions}hasUUID> ?planId . \
        }}",
        actions = ACTIONS_NS,
        bfo = BFO_NS,
        has_part = BFO_HAS_PART,
        cco = CCO_NS,
        plan = CCO_PLAN,
    );

    let rows = execute_select_rows(store, &sparql)?;
    rows.into_iter()
        .map(|row| {
            let charter_id = row
                .get("charterId")
                .ok_or_else(|| GraphError::Domain("Missing charterId in edge row".to_string()))?;
            let plan_id = row
                .get("planId")
                .ok_or_else(|| GraphError::Domain("Missing planId in edge row".to_string()))?;
            Ok((
                parse_uuid(plan_id, "plan")?,
                parse_uuid(charter_id, "charter")?,
            ))
        })
        .collect()
}

fn get_charter_by_id(store: &Store, id: Uuid) -> Result<Charter> {
    let node = NodeView::new(store, id);
    let title = node
        .lit(rdfs_pred(RDFS_LABEL))
        .ok_or_else(|| GraphError::Domain(format!("Charter {} missing title", id)))?;
    let alias = node.lit(actions_pred("hasAlias"));

    Ok(Charter {
        id,
        title,
        description: node.lit(rdfs_pred(RDFS_COMMENT)),
        alias,
        parent: query_charter_parent_alias_or_title(store, id)?,
        ..Default::default()
    })
}

fn query_charter_parent_alias_or_title(
    store: &Store,
    child_id: Uuid,
) -> Result<Option<String>> {
    let q = format!(
        "SELECT ?pid WHERE {{ \
            ?parent a <{actions}Charter> ; <{actions}hasSubCharter> <urn:uuid:{child}> ; <{actions}hasUUID> ?pid . \
        }}",
        actions = ACTIONS_NS,
        child = child_id,
    );
    let parent_ids = query_uuids(store, &q, "pid", "parent charter")?;
    let Some(parent_id) = parent_ids.first().copied() else {
        return Ok(None);
    };

    let parent = NodeView::new(store, parent_id);
    let parent_title = parent
        .lit(rdfs_pred(RDFS_LABEL))
        .ok_or_else(|| GraphError::Domain(format!("Charter {} missing title", parent_id)))?;
    Ok(parent.lit(actions_pred("hasAlias")).or(Some(parent_title)))
}

fn query_ids(store: &Store, sparql: &str, var_name: &str) -> Result<Vec<String>> {
    Ok(execute_select_rows(store, sparql)?
        .into_iter()
        .filter_map(|row| row.get(var_name).cloned())
        .collect())
}

fn query_uuids(
    store: &Store,
    sparql: &str,
    var_name: &str,
    kind: &str,
) -> Result<Vec<Uuid>> {
    query_ids(store, sparql, var_name)?
        .into_iter()
        .map(|id| parse_uuid(&id, kind))
        .collect()
}

fn query_term_values(store: &Store, sparql: &str, var_name: &str) -> Result<Vec<String>> {
    query_ids(store, sparql, var_name)
}

fn execute_select_rows(store: &Store, sparql: &str) -> Result<Vec<Row>> {
    let results = SparqlEvaluator::new()
        .parse_query(sparql)
        .map_err(|e| GraphError::Query(e.to_string()))?
        .on_store(store)
        .execute()
        .map_err(|e| GraphError::Query(e.to_string()))?;

    match results {
        QueryResults::Solutions(solutions) => {
            let var_names: Vec<String> = solutions
                .variables()
                .iter()
                .map(|v| v.as_str().to_string())
                .collect();
            let mut rows = Vec::new();
            for solution in solutions {
                let solution = solution.map_err(|e| GraphError::Query(e.to_string()))?;
                let mut row = HashMap::new();
                for var_name in &var_names {
                    if let Some(term) = solution.get(var_name.as_str()) {
                        row.insert(var_name.clone(), stringify_term(term));
                    }
                }
                rows.push(row);
            }
            Ok(rows)
        }
        QueryResults::Boolean(_) => Err(GraphError::Query("ASK queries not supported; use SELECT".to_string())),
        QueryResults::Graph(_) => Err(GraphError::Query("CONSTRUCT/DESCRIBE not supported; use SELECT".to_string())),
    }
}

fn stringify_term(term: &Term) -> String {
    match term {
        Term::NamedNode(nn) => nn.as_str().to_string(),
        Term::Literal(lit) => lit.value().to_string(),
        Term::BlankNode(bn) => format!("_:{}", bn.as_str()),
    }
}

fn parse_uuid(value: &str, kind: &str) -> Result<Uuid> {
    Uuid::parse_str(value).map_err(|e| GraphError::Domain(format!("Invalid {kind} UUID '{}': {}", value, e)))
}

struct NodeView<'a> {
    store: &'a Store,
    subject: NamedOrBlankNode,
}

impl<'a> NodeView<'a> {
    fn new(store: &'a Store, id: Uuid) -> Self {
        Self {
            store,
            subject: NamedOrBlankNode::NamedNode(
                NamedNode::new(format!("urn:uuid:{}", id)).unwrap(),
            ),
        }
    }

    fn one(&self, pred: NamedNode) -> Option<Term> {
        self.store
            .quads_for_pattern(
                Some(self.subject.as_ref()),
                Some(pred.as_ref()),
                None,
                Some(GraphName::DefaultGraph.as_ref()),
            )
            .next()
            .and_then(|r| r.ok())
            .map(|q| q.object)
    }

    fn many(&self, pred: NamedNode) -> Vec<Term> {
        self.store
            .quads_for_pattern(
                Some(self.subject.as_ref()),
                Some(pred.as_ref()),
                None,
                Some(GraphName::DefaultGraph.as_ref()),
            )
            .filter_map(|r| r.ok())
            .map(|q| q.object)
            .collect()
    }

    fn lit(&self, pred: NamedNode) -> Option<String> {
        match self.one(pred) {
            Some(Term::Literal(lit)) => Some(lit.value().to_string()),
            _ => None,
        }
    }

    fn lit_u32(&self, pred: NamedNode) -> Option<u32> {
        self.lit(pred).and_then(|s| s.parse::<u32>().ok())
    }

    fn lit_bool(&self, pred: NamedNode) -> Option<bool> {
        self.lit(pred).and_then(|s| s.parse::<bool>().ok())
    }

    fn datetime(&self, pred: NamedNode) -> Option<DateTime<Local>> {
        self.lit(pred)
            .and_then(|value| DateTime::parse_from_rfc3339(&value).ok())
            .map(|dt| dt.with_timezone(&Local))
    }

    fn uuid_node(&self, pred: NamedNode) -> Option<Uuid> {
        self.one(pred).and_then(term_uuid_urn)
    }

    fn uuid_nodes(&self, pred: NamedNode) -> Vec<Uuid> {
        self.many(pred)
            .into_iter()
            .filter_map(term_uuid_urn)
            .collect()
    }
}

fn term_uuid_urn(term: Term) -> Option<Uuid> {
    match term {
        Term::NamedNode(nn) => nn
            .as_str()
            .strip_prefix("urn:uuid:")
            .and_then(|value| Uuid::parse_str(value).ok()),
        _ => None,
    }
}

fn get_plan_by_id(store: &Store, id: Uuid) -> Result<Plan> {
    let node = NodeView::new(store, id);
    let name = node
        .lit(rdfs_pred(RDFS_LABEL))
        .ok_or_else(|| GraphError::Domain(format!("Plan {} missing name", id)))?;

    let contexts: Vec<String> = node
        .many(actions_pred("hasContext"))
        .into_iter()
        .filter_map(|term| match term {
            Term::Literal(lit) => Some(lit.value().to_string()),
            _ => None,
        })
        .collect();

    let mut depends_on = node.uuid_nodes(cco_node(CCO_IS_SUCCESSOR_OF));
    depends_on.sort_unstable();
    depends_on.dedup();

    Ok(Plan {
        id,
        name,
        description: node.lit(rdfs_pred(RDFS_COMMENT)),
        priority: node.lit_u32(actions_pred("hasPriority")),
        contexts: (!contexts.is_empty()).then_some(contexts),
        recurrence: node
            .lit(actions_pred("hasRecurrenceRule"))
            .as_deref()
            .and_then(parse_recurrence_rule),
        due_recurrence: node
            .lit(actions_pred("hasDueRecurrenceRule"))
            .as_deref()
            .and_then(parse_recurrence_rule),
        parent: node.uuid_node(bfo_pred(BFO_PART_OF)),
        alias: node.lit(actions_pred("hasAlias")),
        is_sequential: node.lit_bool(actions_pred("hasSequentialChildren")),
        depends_on: (!depends_on.is_empty()).then_some(depends_on),
        acts: vec![],
        sub_plans: vec![],
    })
}

fn parse_recurrence_rule(rrule: &str) -> Option<Recurrence> {
    let mut frequency = None;
    let mut interval = None;
    let mut by_day = Vec::new();

    for part in rrule.split(';') {
        let (key, value) = part.split_once('=')?;
        match key {
            "FREQ" => frequency = Some(value.to_lowercase()),
            "INTERVAL" => interval = value.parse::<u32>().ok(),
            "BYDAY" => {
                by_day.extend(
                    value
                        .split(',')
                        .filter(|day| !day.is_empty())
                        .map(str::to_string),
                );
            }
            _ => {}
        }
    }

    Some(Recurrence {
        frequency: frequency?,
        interval,
        count: None,
        until: None,
        by_second: None,
        by_minute: None,
        by_hour: None,
        by_day: if by_day.is_empty() {
            None
        } else {
            Some(by_day)
        },
        by_month_day: None,
        by_year_day: None,
        by_week_no: None,
        by_month: None,
        by_set_pos: None,
        week_start: None,
    })
}

fn get_planned_act_by_id(store: &Store, id: Uuid) -> Result<PlannedAct> {
    let node = NodeView::new(store, id);
    let plan_id = node
        .uuid_node(cco_node(CCO_PRESCRIBED_BY))
        .ok_or_else(|| GraphError::Domain(format!("PlannedAct {} missing prescribedBy", id)))?;

    let phase = match node.one(cco_node(CCO_STATUS_PROP)) {
        Some(Term::NamedNode(nn)) => match nn.as_str().split('#').next_back() {
            Some("Completed") => ActPhase::Completed,
            Some("InProgress") => ActPhase::InProgress,
            Some("Blocked") => ActPhase::Blocked,
            Some("Cancelled") => ActPhase::Cancelled,
            _ => ActPhase::NotStarted,
        },
        _ => ActPhase::NotStarted,
    };

    Ok(PlannedAct {
        id,
        plan_id,
        phase,
        scheduled_at: node.datetime(actions_pred("hasScheduledDateTime")),
        due_date: node.datetime(actions_pred("hasDueDateTime")),
        duration: node.lit_u32(actions_pred("hasDurationMinutes")),
        completed_at: node.datetime(actions_pred("hasCompletedDateTime")),
        created_at: node.datetime(actions_pred("hasCreatedDateTime")),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::{create_store, load_domain_model};

    #[test]
    fn store_roundtrip_reconstructs_domain_model() {
        let store = create_store().expect("store");

        let charter_id = Uuid::parse_str("019d7100-3333-7333-8333-333333333333").unwrap();
        let plan_id = Uuid::parse_str("019d7100-1111-7111-8111-111111111111").unwrap();
        let act_id = Uuid::parse_str("019d7100-2222-7222-8222-222222222222").unwrap();

        let model = DomainModel {
            objectives: vec![],
            charters: vec![Charter {
                id: charter_id,
                title: "Platform".to_string(),
                description: Some("Platform charter".to_string()),
                alias: Some("platform".to_string()),
                plans: vec![Plan {
                    id: plan_id,
                    name: "Write graph tests".to_string(),
                    description: Some("Lock down graph semantics".to_string()),
                    priority: Some(1),
                    contexts: Some(vec!["dev".to_string()]),
                    recurrence: Some(Recurrence {
                        frequency: "weekly".to_string(),
                        interval: Some(2),
                        by_day: Some(vec!["MO".to_string(), "WE".to_string()]),
                        ..Default::default()
                    }),
                    alias: Some("graph_tests".to_string()),
                    is_sequential: Some(true),
                    depends_on: Some(vec![Uuid::parse_str(
                        "019d7100-4444-7444-8444-444444444444",
                    )
                    .unwrap()]),
                    acts: vec![PlannedAct {
                        id: act_id,
                        plan_id,
                        phase: ActPhase::InProgress,
                        scheduled_at: Some(chrono::Local::now()),
                        duration: Some(45),
                        created_at: Some(chrono::Local::now()),
                        ..Default::default()
                    }],
                    ..Default::default()
                }],
                ..Default::default()
            }],
        };

        load_domain_model(&store, &model).expect("load domain model");
        let rebuilt = load_domain_model_from_store(&store).expect("rebuild from store");

        assert_eq!(rebuilt.charters.len(), 1);
        assert_eq!(rebuilt.charters[0].title, "Platform");
        assert_eq!(rebuilt.charters[0].plans.len(), 1);
        assert_eq!(rebuilt.charters[0].plans[0].name, "Write graph tests");
        assert_eq!(rebuilt.charters[0].plans[0].acts.len(), 1);
        assert_eq!(rebuilt.charters[0].plans[0].acts[0].id, act_id);
    }

    #[test]
    fn build_raw_where_query_injects_standard_prefixes() {
        let q = build_raw_where_query("?s ?p ?o .");
        assert!(q.contains("PREFIX actions:"));
        assert!(q.contains("PREFIX cco:"));
        assert!(q.contains("SELECT * WHERE"));
    }

    #[test]
    fn query_action_ids_reads_id_literal_bindings() {
        let store = create_store().expect("store");
        let ttl = format!(
            "@prefix actions: <{actions}> .\n\
             @prefix cco: <{cco}> .\n\
             <urn:uuid:019d7100-1111-7111-8111-111111111111> a cco:{plan} ;\n\
               actions:hasUUID \"019d7100-1111-7111-8111-111111111111\" .\n",
            actions = ACTIONS_NS,
            cco = CCO_NS,
            plan = CCO_PLAN,
        );
        crate::graph::load_turtle(&store, &ttl).expect("load turtle");

        let sparql = format!(
            "SELECT ?id WHERE {{ ?s a <{cco}{plan}> ; <{actions}hasUUID> ?id . }}",
            cco = CCO_NS,
            plan = CCO_PLAN,
            actions = ACTIONS_NS,
        );

        let ids = query_action_ids(&store, &sparql).expect("query ids");
        assert_eq!(ids, vec!["019d7100-1111-7111-8111-111111111111"]);
    }

    #[test]
    fn validate_actions_detects_missing_status() {
        let store = create_store().expect("store");
        let ttl = format!(
            "@prefix actions: <{actions}> .\n\
             @prefix cco: <{cco}> .\n\
             <urn:uuid:019d7100-2222-7222-8222-222222222222> a cco:{planned_act} ;\n\
               actions:hasUUID \"019d7100-2222-7222-8222-222222222222\" ;\n\
               cco:{prescribed_by} <urn:uuid:019d7100-1111-7111-8111-111111111111> .\n",
            actions = ACTIONS_NS,
            cco = CCO_NS,
            planned_act = CCO_PLANNED_ACT,
            prescribed_by = CCO_PRESCRIBED_BY,
        );
        crate::graph::load_turtle(&store, &ttl).expect("load turtle");
        let violations = validate_actions_vocabulary(&store).expect("validate");
        assert!(violations.iter().any(|v| v.contains("missing a status")));
    }

    #[test]
    fn recurrence_parser_handles_weekly_byday() {
        let parsed =
            parse_recurrence_rule("FREQ=WEEKLY;INTERVAL=2;BYDAY=MO,WE").expect("parse recurrence");
        assert_eq!(parsed.frequency, "weekly");
        assert_eq!(parsed.interval, Some(2));
        assert_eq!(
            parsed.by_day,
            Some(vec!["MO".to_string(), "WE".to_string()])
        );
    }

    #[test]
    fn charter_parent_uses_parent_alias_or_title() {
        let store = create_store().expect("store");

        let ttl = format!(
            "@prefix actions: <{actions}> .\n\
             @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n\
             <urn:uuid:019d7100-aaaa-7aaa-8aaa-aaaaaaaaaaaa> a actions:Charter ;\n\
               actions:hasUUID \"019d7100-aaaa-7aaa-8aaa-aaaaaaaaaaaa\" ;\n\
               rdfs:label \"Parent Charter\" ;\n\
               actions:hasAlias \"parent\" ;\n\
               actions:hasSubCharter <urn:uuid:019d7100-bbbb-7bbb-8bbb-bbbbbbbbbbbb> .\n\
             <urn:uuid:019d7100-bbbb-7bbb-8bbb-bbbbbbbbbbbb> a actions:Charter ;\n\
               actions:hasUUID \"019d7100-bbbb-7bbb-8bbb-bbbbbbbbbbbb\" ;\n\
               rdfs:label \"Child Charter\" .\n",
            actions = ACTIONS_NS,
        );

        crate::graph::load_turtle(&store, &ttl).expect("load turtle");
        let model = load_domain_model_from_store(&store).expect("load model");

        let child = model
            .charters
            .iter()
            .find(|c| c.title == "Child Charter")
            .expect("child charter");

        assert_eq!(child.parent.as_deref(), Some("parent"));
    }

    #[test]
    fn graph_roundtrip_keeps_single_copy_of_dependency_links() {
        let store = create_store().expect("store");
        let plan_id = Uuid::parse_str("019d7100-1111-7111-8111-111111111111").unwrap();
        let dep_id = Uuid::parse_str("019d7100-4444-7444-8444-444444444444").unwrap();

        let ttl = format!(
            "@prefix actions: <{actions}> .\n\
             @prefix cco: <{cco}> .\n\
             @prefix bfo: <{bfo}> .\n\
             @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n\
            <urn:uuid:019d7100-3333-7333-8333-333333333333> a actions:Charter ;\n\
              actions:hasUUID \"019d7100-3333-7333-8333-333333333333\" ;\n\
              rdfs:label \"Root\" ;\n\
              bfo:{has_part} <urn:uuid:{plan}> .\n\
             <urn:uuid:{plan}> a cco:{plan_class} ;\n\
               actions:hasUUID \"{plan}\" ;\n\
               rdfs:label \"Plan\" ;\n\
               cco:{succ} <urn:uuid:{dep}> ;\n\
               cco:{succ} <urn:uuid:{dep}> .\n",
            actions = ACTIONS_NS,
            cco = CCO_NS,
            bfo = BFO_NS,
            has_part = BFO_HAS_PART,
            plan = plan_id,
            dep = dep_id,
            plan_class = CCO_PLAN,
            succ = CCO_IS_SUCCESSOR_OF,
        );
        crate::graph::load_turtle(&store, &ttl).expect("load turtle");

        let model = load_domain_model_from_store(&store).expect("load model");
        let plan = model.all_plans()[0];
        let deps = plan.depends_on.clone().expect("depends_on");
        assert_eq!(deps, vec![dep_id]);
    }

    #[test]
    fn ignores_orphan_plans_not_linked_to_charter() {
        let store = create_store().expect("store");
        let ttl = format!(
            "@prefix actions: <{actions}> .\n\
             @prefix cco: <{cco}> .\n\
             @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n\
             <urn:uuid:019d7100-1111-7111-8111-111111111111> a cco:{plan_class} ;\n\
               actions:hasUUID \"019d7100-1111-7111-8111-111111111111\" ;\n\
               rdfs:label \"Orphan plan\" .\n",
            actions = ACTIONS_NS,
            cco = CCO_NS,
            plan_class = CCO_PLAN,
        );
        crate::graph::load_turtle(&store, &ttl).expect("load turtle");
        let model = load_domain_model_from_store(&store).expect("load model");
        assert!(model.all_plans().is_empty());
    }

    #[test]
    fn reads_due_date_and_due_recurrence() {
        let store = create_store().expect("store");
        let plan_id = Uuid::parse_str("019d7100-1111-7111-8111-111111111111").unwrap();
        let act_id = Uuid::parse_str("019d7100-2222-7222-8222-222222222222").unwrap();

        let ttl = format!(
            "@prefix actions: <{actions}> .\n\
             @prefix cco: <{cco}> .\n\
             @prefix bfo: <{bfo}> .\n\
             @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n\
             @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n\
             <urn:uuid:019d7100-3333-7333-8333-333333333333> a actions:Charter ;\n\
               actions:hasUUID \"019d7100-3333-7333-8333-333333333333\" ;\n\
               rdfs:label \"Root\" ;\n\
               bfo:{has_part} <urn:uuid:{plan}> .\n\
             <urn:uuid:{plan}> a cco:{plan_class} ;\n\
               actions:hasUUID \"{plan}\" ;\n\
               rdfs:label \"Plan\" ;\n\
               actions:hasDueRecurrenceRule \"FREQ=DAILY\" ;\n\
               cco:{prescribes} <urn:uuid:{act}> .\n\
             <urn:uuid:{act}> a cco:{planned_act} ;\n\
               actions:hasUUID \"{act}\" ;\n\
               cco:{prescribed_by} <urn:uuid:{plan}> ;\n\
               cco:{status} actions:NotStarted ;\n\
               actions:hasDueDateTime \"2026-04-12T09:00:00+00:00\"^^xsd:dateTime .\n",
            actions = ACTIONS_NS,
            cco = CCO_NS,
            bfo = BFO_NS,
            has_part = BFO_HAS_PART,
            plan = plan_id,
            act = act_id,
            plan_class = CCO_PLAN,
            prescribes = CCO_PRESCRIBES,
            planned_act = CCO_PLANNED_ACT,
            prescribed_by = CCO_PRESCRIBED_BY,
            status = CCO_STATUS_PROP,
        );
        crate::graph::load_turtle(&store, &ttl).expect("load turtle");

        let model = load_domain_model_from_store(&store).expect("load model");
        let plan = model.all_plans()[0];
        let act = model.all_acts()[0];
        assert_eq!(
            plan.due_recurrence.as_ref().map(|r| r.frequency.as_str()),
            Some("daily")
        );
        assert!(act.due_date.is_some());
    }

    #[test]
    fn raw_query_errors_on_boolean_queries() {
        let store = create_store().expect("store");
        let err = query_raw(&store, "ASK { ?s ?p ?o }").expect_err("expected error");
        assert!(err.to_string().contains("ASK queries not supported"));
    }

    #[test]
    fn validates_prescribes_target_type() {
        let store = create_store().expect("store");
        let ttl = format!(
            "@prefix actions: <{actions}> .\n\
             @prefix cco: <{cco}> .\n\
             @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n\
             <urn:uuid:019d7100-1111-7111-8111-111111111111> a cco:{plan_class} ;\n\
               actions:hasUUID \"019d7100-1111-7111-8111-111111111111\" ;\n\
               rdfs:label \"Plan\" ;\n\
               cco:{prescribes} <urn:uuid:019d7100-9999-7999-8999-999999999999> .\n",
            actions = ACTIONS_NS,
            cco = CCO_NS,
            plan_class = CCO_PLAN,
            prescribes = CCO_PRESCRIBES,
        );
        crate::graph::load_turtle(&store, &ttl).expect("load turtle");
        let violations = validate_actions_vocabulary(&store).expect("validate");
        assert!(violations.iter().any(|v| v.contains("PlanPrescribesShape")));
    }

    #[test]
    fn empty_store_roundtrips_to_empty_domain_model() {
        let store = create_store().expect("store");
        let model = load_domain_model_from_store(&store).expect("load model");
        assert!(model.charters.is_empty());
        assert!(model.objectives.is_empty());
    }

    #[test]
    fn plan_without_name_returns_error() {
        let store = create_store().expect("store");
        let ttl = format!(
            "@prefix actions: <{actions}> .\n\
             @prefix cco: <{cco}> .\n\
             <urn:uuid:019d7100-1111-7111-8111-111111111111> a cco:{plan_class} ;\n\
               actions:hasUUID \"019d7100-1111-7111-8111-111111111111\" .\n",
            actions = ACTIONS_NS,
            cco = CCO_NS,
            plan_class = CCO_PLAN,
        );
        crate::graph::load_turtle(&store, &ttl).expect("load turtle");

        let err = query_plans(&store).expect_err("expected error");
        assert!(err.to_string().contains("missing name"));
    }

    #[test]
    fn recursive_parent_lookup_does_not_loop() {
        let store = create_store().expect("store");
        let a = Uuid::parse_str("019d7100-aaaa-7aaa-8aaa-aaaaaaaaaaaa").unwrap();
        let b = Uuid::parse_str("019d7100-bbbb-7bbb-8bbb-bbbbbbbbbbbb").unwrap();
        let ttl = format!(
            "@prefix actions: <{actions}> .\n\
             @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n\
             <urn:uuid:{a}> a actions:Charter ; actions:hasUUID \"{a}\" ; rdfs:label \"A\" ; actions:hasSubCharter <urn:uuid:{b}> .\n\
             <urn:uuid:{b}> a actions:Charter ; actions:hasUUID \"{b}\" ; rdfs:label \"B\" ; actions:hasSubCharter <urn:uuid:{a}> .\n",
            actions = ACTIONS_NS,
            a = a,
            b = b,
        );
        crate::graph::load_turtle(&store, &ttl).expect("load turtle");

        let model = load_domain_model_from_store(&store).expect("load model");
        assert_eq!(model.charters.len(), 2);
        let titles: std::collections::HashSet<String> =
            model.charters.iter().map(|c| c.title.clone()).collect();
        assert!(titles.contains("A"));
        assert!(titles.contains("B"));
    }
}
