use crate::domain::{Charter, DomainModel, Plan, PlannedAct};
use std::fmt;
use uuid::Uuid;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReferenceTarget {
    Charter(Uuid),
    Plan(Uuid),
    Act(Uuid),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MatchMode {
    Exact,
}

#[derive(Debug, Clone, Copy)]
pub struct ReferenceOptions {
    pub allow_prefixes: bool,
    pub match_mode: MatchMode,
}

impl Default for ReferenceOptions {
    fn default() -> Self {
        Self {
            allow_prefixes: true,
            match_mode: MatchMode::Exact,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ReferenceError {
    message: String,
}

impl ReferenceError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl fmt::Display for ReferenceError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for ReferenceError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Prefix {
    Charter,
    Plan,
    Act,
}

#[derive(Debug, Clone, Copy)]
enum Scope<'a> {
    Charter(&'a Charter),
    Plan(&'a Charter, &'a Plan),
}

pub fn resolve_reference(
    model: &DomainModel,
    input: &str,
    options: &ReferenceOptions,
) -> Result<ReferenceTarget, ReferenceError> {
    let trimmed = input.trim();
    if trimmed.is_empty() {
        return Err(ReferenceError::new("Reference cannot be empty"));
    }

    let (prefix, path) = parse_prefix(trimmed, options.allow_prefixes)?;
    let segments = split_segments(path)?;

    match prefix {
        Some(Prefix::Charter) => {
            let target = resolve_path(model, &segments)?;
            match target {
                ReferenceTarget::Charter(_) => Ok(target),
                _ => Err(ReferenceError::new(
                    "Reference resolved to a non-charter target; use a charter alias or UUID",
                )),
            }
        }
        Some(Prefix::Plan) => {
            if segments.len() == 1 {
                return resolve_plan_global(model, segments[0]);
            }
            let target = resolve_path(model, &segments)?;
            match target {
                ReferenceTarget::Plan(_) => Ok(target),
                _ => Err(ReferenceError::new(
                    "Reference resolved to a non-plan target; use a plan alias or UUID",
                )),
            }
        }
        Some(Prefix::Act) => {
            if segments.len() == 1 {
                return resolve_act_global(model, segments[0]);
            }
            let target = resolve_path(model, &segments)?;
            match target {
                ReferenceTarget::Act(_) => Ok(target),
                _ => Err(ReferenceError::new(
                    "Reference resolved to a non-act target; use an act UUID",
                )),
            }
        }
        None => {
            if segments.len() == 1 {
                return resolve_unscoped_single(model, segments[0]);
            }
            resolve_path(model, &segments)
        }
    }
}

pub fn filter_model_for_charter(
    model: &DomainModel,
    charter_id: Uuid,
    recursive: bool,
) -> DomainModel {
    if !recursive {
        let charters = model
            .charters
            .iter()
            .filter(|c| c.id == charter_id)
            .cloned()
            .collect();
        return DomainModel {
            objectives: vec![],
            charters,
        };
    }

    let mut to_visit = vec![charter_id];
    let mut keep = std::collections::HashSet::new();

    while let Some(current) = to_visit.pop() {
        if !keep.insert(current) {
            continue;
        }
        if let Some(parent) = model.charters.iter().find(|c| c.id == current) {
            for child in model
                .charters
                .iter()
                .filter(|c| is_child_charter(c, parent))
            {
                if !keep.contains(&child.id) {
                    to_visit.push(child.id);
                }
            }
        }
    }

    let charters = model
        .charters
        .iter()
        .filter(|c| keep.contains(&c.id))
        .cloned()
        .collect();

    DomainModel {
        objectives: vec![],
        charters,
    }
}

pub fn filter_model_for_plan(model: &DomainModel, plan_id: Uuid) -> DomainModel {
    for charter in &model.charters {
        if let Some(plan) = charter.plans.iter().find(|p| p.id == plan_id) {
            let mut charter_copy = charter.clone();
            charter_copy.plans = vec![plan.clone()];
            return DomainModel {
                objectives: vec![],
                charters: vec![charter_copy],
            };
        }
    }

    DomainModel {
        objectives: vec![],
        charters: vec![],
    }
}

pub fn filter_model_for_act(model: &DomainModel, act_id: Uuid) -> DomainModel {
    for charter in &model.charters {
        for plan in &charter.plans {
            if let Some(act) = plan.acts.iter().find(|a| a.id == act_id) {
                let mut plan_copy = plan.clone();
                plan_copy.acts = vec![act.clone()];
                let mut charter_copy = charter.clone();
                charter_copy.plans = vec![plan_copy];
                return DomainModel {
                    objectives: vec![],
                    charters: vec![charter_copy],
                };
            }
        }
    }

    DomainModel {
        objectives: vec![],
        charters: vec![],
    }
}

fn parse_prefix(
    input: &str,
    allow_prefixes: bool,
) -> Result<(Option<Prefix>, &str), ReferenceError> {
    if !allow_prefixes {
        return Ok((None, input));
    }

    if input.len() < 2 {
        return Ok((None, input));
    }

    let prefix = match &input[..2].to_ascii_lowercase()[..] {
        "c:" => Some(Prefix::Charter),
        "p:" => Some(Prefix::Plan),
        "a:" => Some(Prefix::Act),
        _ => None,
    };

    if let Some(found) = prefix {
        let rest = input[2..].trim();
        if rest.is_empty() {
            return Err(ReferenceError::new(
                "Reference prefix provided without a value",
            ));
        }
        Ok((Some(found), rest))
    } else {
        Ok((None, input))
    }
}

fn split_segments(path: &str) -> Result<Vec<&str>, ReferenceError> {
    let segments: Vec<&str> = path.split('/').filter(|s| !s.is_empty()).collect();
    if segments.is_empty() {
        return Err(ReferenceError::new("Reference path is empty"));
    }
    Ok(segments)
}

fn resolve_unscoped_single(
    model: &DomainModel,
    segment: &str,
) -> Result<ReferenceTarget, ReferenceError> {
    if let Ok(target) = resolve_charter_global(model, segment) {
        return Ok(target);
    }
    if let Ok(target) = resolve_plan_global(model, segment) {
        return Ok(target);
    }
    resolve_act_global(model, segment)
}

fn resolve_charter_global(
    model: &DomainModel,
    segment: &str,
) -> Result<ReferenceTarget, ReferenceError> {
    let matches = model
        .charters
        .iter()
        .filter(|c| charter_matches_segment(c, segment))
        .collect::<Vec<_>>();

    match matches.len() {
        0 => Err(ReferenceError::new(format!(
            "No charter matches reference '{}'",
            segment
        ))),
        1 => Ok(ReferenceTarget::Charter(matches[0].id)),
        _ => Err(ReferenceError::new(format!(
            "Ambiguous charter reference '{}'; use c:<alias> or c:<uuid>",
            segment
        ))),
    }
}

fn resolve_plan_global(
    model: &DomainModel,
    segment: &str,
) -> Result<ReferenceTarget, ReferenceError> {
    let mut matches: Vec<&Plan> = Vec::new();
    for charter in &model.charters {
        for plan in &charter.plans {
            if plan_matches_segment(plan, segment) {
                matches.push(plan);
            }
        }
    }

    match matches.len() {
        0 => Err(ReferenceError::new(format!(
            "No plan matches reference '{}'",
            segment
        ))),
        1 => Ok(ReferenceTarget::Plan(matches[0].id)),
        _ => Err(ReferenceError::new(format!(
            "Ambiguous plan reference '{}'; use p:<alias> with a charter path or a UUID",
            segment
        ))),
    }
}

fn resolve_act_global(
    model: &DomainModel,
    segment: &str,
) -> Result<ReferenceTarget, ReferenceError> {
    let mut matches: Vec<&PlannedAct> = Vec::new();
    for charter in &model.charters {
        for plan in &charter.plans {
            for act in &plan.acts {
                if act_matches_segment(act, segment) {
                    matches.push(act);
                }
            }
        }
    }

    match matches.len() {
        0 => Err(ReferenceError::new(format!(
            "No planned act matches reference '{}'",
            segment
        ))),
        1 => Ok(ReferenceTarget::Act(matches[0].id)),
        _ => Err(ReferenceError::new(format!(
            "Ambiguous act reference '{}'; use a full UUID",
            segment
        ))),
    }
}

fn resolve_path(model: &DomainModel, segments: &[&str]) -> Result<ReferenceTarget, ReferenceError> {
    let first = segments
        .first()
        .ok_or_else(|| ReferenceError::new("Reference path is empty"))?;

    let root_matches: Vec<&Charter> = model
        .charters
        .iter()
        .filter(|c| is_root_charter(c) && charter_matches_segment(c, first))
        .collect();

    let mut scope = match root_matches.len() {
        0 => {
            return Err(ReferenceError::new(format!(
                "No charter matches root reference '{}'",
                first
            )))
        }
        1 => Scope::Charter(root_matches[0]),
        _ => {
            return Err(ReferenceError::new(format!(
                "Ambiguous root charter reference '{}'; use c:<alias> or c:<uuid>",
                first
            )))
        }
    };

    for segment in &segments[1..] {
        scope = match scope {
            Scope::Charter(charter) => {
                let child_charters: Vec<&Charter> = model
                    .charters
                    .iter()
                    .filter(|c| is_child_charter(c, charter) && charter_matches_segment(c, segment))
                    .collect();

                let child_plans: Vec<&Plan> = charter
                    .plans
                    .iter()
                    .filter(|p| p.parent.is_none() && plan_matches_segment(p, segment))
                    .collect();

                match (child_charters.len(), child_plans.len()) {
                    (0, 0) => {
                        return Err(ReferenceError::new(format!(
                            "No match for '{}' under charter '{}'",
                            segment, charter.title
                        )))
                    }
                    (1, 0) => Scope::Charter(child_charters[0]),
                    (0, 1) => Scope::Plan(charter, child_plans[0]),
                    _ => {
                        return Err(ReferenceError::new(format!(
                            "Ambiguous reference '{}' under charter '{}'; use c: or p: prefix",
                            segment, charter.title
                        )))
                    }
                }
            }
            Scope::Plan(charter, plan) => {
                let child_plans: Vec<&Plan> = charter
                    .plans
                    .iter()
                    .filter(|p| p.parent == Some(plan.id) && plan_matches_segment(p, segment))
                    .collect();

                let child_acts: Vec<&PlannedAct> = plan
                    .acts
                    .iter()
                    .filter(|a| act_matches_segment(a, segment))
                    .collect();

                match (child_plans.len(), child_acts.len()) {
                    (0, 0) => {
                        return Err(ReferenceError::new(format!(
                            "No match for '{}' under plan '{}'",
                            segment, plan.name
                        )))
                    }
                    (1, 0) => Scope::Plan(charter, child_plans[0]),
                    (0, 1) => return Ok(ReferenceTarget::Act(child_acts[0].id)),
                    _ => {
                        return Err(ReferenceError::new(format!(
                            "Ambiguous reference '{}' under plan '{}'; use p: or a: prefix",
                            segment, plan.name
                        )))
                    }
                }
            }
        };
    }

    match scope {
        Scope::Charter(charter) => Ok(ReferenceTarget::Charter(charter.id)),
        Scope::Plan(_, plan) => Ok(ReferenceTarget::Plan(plan.id)),
    }
}

fn is_root_charter(charter: &Charter) -> bool {
    match &charter.parent {
        None => true,
        Some(parent) => parent.trim().is_empty(),
    }
}

fn is_child_charter(child: &Charter, parent: &Charter) -> bool {
    let parent_ref = match &child.parent {
        Some(value) => value.trim().to_lowercase(),
        None => return false,
    };

    if parent_ref.is_empty() {
        return false;
    }

    if let Some(alias) = &parent.alias {
        if parent_ref == alias.to_lowercase() {
            return true;
        }
    }

    let parent_id = parent.id.to_string();
    if parent_ref == parent_id.to_lowercase() {
        return true;
    }

    let short = &parent_id[..8];
    parent_ref == short
}

fn charter_matches_segment(charter: &Charter, segment: &str) -> bool {
    if matches_uuid(&charter.id, segment) {
        return true;
    }

    match &charter.alias {
        Some(alias) => alias_match(alias, segment),
        None => false,
    }
}

fn plan_matches_segment(plan: &Plan, segment: &str) -> bool {
    if matches_uuid(&plan.id, segment) {
        return true;
    }

    match &plan.alias {
        Some(alias) => alias_match(alias, segment),
        None => false,
    }
}

fn act_matches_segment(act: &PlannedAct, segment: &str) -> bool {
    matches_uuid(&act.id, segment)
}

fn matches_uuid(id: &Uuid, segment: &str) -> bool {
    if let Ok(parsed) = Uuid::parse_str(segment) {
        return parsed == *id;
    }

    if is_short_uuid(segment) {
        let id_str = id.to_string();
        return id_str.starts_with(&segment.to_lowercase());
    }

    false
}

fn alias_match(alias: &str, segment: &str) -> bool {
    alias.to_lowercase() == segment.to_lowercase()
}

fn is_short_uuid(segment: &str) -> bool {
    if segment.len() != 8 {
        return false;
    }
    segment.chars().all(|c| c.is_ascii_hexdigit())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_plan(id: Uuid, alias: &str, parent: Option<Uuid>) -> Plan {
        Plan {
            id,
            name: alias.to_string(),
            description: None,
            priority: None,
            contexts: None,
            recurrence: None,
            parent,
            objective: None,
            alias: Some(alias.to_string()),
            is_sequential: None,
            duration: None,
            depends_on: None,
            acts: Vec::new(),
        }
    }

    fn make_act(id: Uuid, plan_id: Uuid) -> PlannedAct {
        PlannedAct {
            id,
            plan_id,
            phase: crate::domain::ActPhase::NotStarted,
            scheduled_at: None,
            duration: None,
            completed_at: None,
            created_at: None,
        }
    }

    fn sample_model() -> DomainModel {
        let charter_id = Uuid::parse_str("12345678-0000-0000-0000-000000000001").unwrap();
        let child_charter_id = Uuid::parse_str("abcdef12-0000-0000-0000-000000000002").unwrap();
        let plan_id = Uuid::parse_str("11223344-0000-0000-0000-000000000003").unwrap();
        let subplan_id = Uuid::parse_str("55667788-0000-0000-0000-000000000004").unwrap();
        let act_id = Uuid::parse_str("deadbeef-0000-0000-0000-000000000005").unwrap();

        let mut plan = make_plan(plan_id, "core", None);
        plan.acts.push(make_act(act_id, plan_id));

        let subplan = make_plan(subplan_id, "resolver", Some(plan_id));

        let charter = Charter {
            id: charter_id,
            title: "Build".to_string(),
            description: None,
            alias: Some("build".to_string()),
            parent: None,
            objectives: None,
            plans: vec![plan, subplan],
        };

        let child_charter = Charter {
            id: child_charter_id,
            title: "Observability".to_string(),
            description: None,
            alias: Some("obs".to_string()),
            parent: Some("build".to_string()),
            objectives: None,
            plans: vec![],
        };

        DomainModel {
            objectives: vec![],
            charters: vec![charter, child_charter],
        }
    }

    #[test]
    fn resolves_charter_alias_case_insensitive() {
        let model = sample_model();
        let target = resolve_reference(&model, "BUILD", &ReferenceOptions::default()).unwrap();
        assert_eq!(
            target,
            ReferenceTarget::Charter(
                Uuid::parse_str("12345678-0000-0000-0000-000000000001").unwrap()
            )
        );
    }

    #[test]
    fn resolves_plan_path() {
        let model = sample_model();
        let target = resolve_reference(&model, "build/core", &ReferenceOptions::default()).unwrap();
        assert_eq!(
            target,
            ReferenceTarget::Plan(Uuid::parse_str("11223344-0000-0000-0000-000000000003").unwrap())
        );
    }

    #[test]
    fn resolves_act_in_plan_path() {
        let model = sample_model();
        let act_id = Uuid::parse_str("deadbeef-0000-0000-0000-000000000005").unwrap();
        let short = &act_id.to_string()[..8];
        let path = format!("build/core/{}", short);
        let target = resolve_reference(&model, &path, &ReferenceOptions::default()).unwrap();
        assert_eq!(target, ReferenceTarget::Act(act_id));
    }

    #[test]
    fn resolves_plan_prefix_globally() {
        let model = sample_model();
        let target = resolve_reference(&model, "p:core", &ReferenceOptions::default()).unwrap();
        assert_eq!(
            target,
            ReferenceTarget::Plan(Uuid::parse_str("11223344-0000-0000-0000-000000000003").unwrap())
        );
    }

    #[test]
    fn rejects_missing_prefix_value() {
        let model = sample_model();
        let err = resolve_reference(&model, "c:", &ReferenceOptions::default()).unwrap_err();
        assert!(err.to_string().contains("prefix"));
    }
}
