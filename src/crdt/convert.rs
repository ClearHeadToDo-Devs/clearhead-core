//! Conversion between domain types and CRDT mirror types.
//!
//! This is the boundary layer that keeps the domain model free of
//! CRDT concerns. All Automerge Reconcile/Hydrate derives live on
//! the `crdt::types` side; domain types remain pure business objects.

use crate::domain::{
    ActPhase, Charter, DomainModel, Metric, Objective, Plan, PlannedAct, Recurrence, Reference,
};

use super::types::{
    SyncActPhase, SyncCharter, SyncMetric, SyncModel, SyncObjective, SyncPlan, SyncPlannedAct,
    SyncRecurrence, SyncReference,
};

// ============================================================================
// Domain → CRDT
// ============================================================================

impl From<&DomainModel> for SyncModel {
    fn from(dm: &DomainModel) -> Self {
        SyncModel {
            objectives: dm.objectives.iter().map(SyncObjective::from).collect(),
            charters: dm.charters.iter().map(SyncCharter::from).collect(),
        }
    }
}

impl From<&Objective> for SyncObjective {
    fn from(o: &Objective) -> Self {
        SyncObjective {
            id: o.id,
            title: o.title.clone(),
            description: o.description.clone(),
            alias: o.alias.clone(),
            parent: o.parent.clone(),
            metrics: o
                .metrics
                .as_ref()
                .map(|ms| ms.iter().map(SyncMetric::from).collect()),
        }
    }
}

impl From<&Metric> for SyncMetric {
    fn from(m: &Metric) -> Self {
        SyncMetric {
            name: m.name.clone(),
            description: m.description.clone(),
            target: m.target.clone(),
            review_date: m.review_date.clone(),
        }
    }
}

impl From<&Reference> for SyncReference {
    fn from(r: &Reference) -> Self {
        match r {
            Reference::UUID(id) => SyncReference::UUID(*id),
            Reference::Prefix(s) => SyncReference::Prefix(s.clone()),
            Reference::Name(s) => SyncReference::Name(s.clone()),
            Reference::Alias(s) => SyncReference::Alias(s.clone()),
        }
    }
}

impl From<&Charter> for SyncCharter {
    fn from(c: &Charter) -> Self {
        SyncCharter {
            id: c.id,
            title: c.title.clone(),
            description: c.description.clone(),
            alias: c.alias.clone(),
            parent: c.parent.clone(),
            objectives: c.objectives.clone(),
            plans: c.plans.iter().map(SyncPlan::from).collect(),
        }
    }
}

impl From<&Plan> for SyncPlan {
    fn from(p: &Plan) -> Self {
        SyncPlan {
            id: p.id,
            name: p.name.clone(),
            description: p.description.clone(),
            priority: p.priority,
            contexts: p.contexts.clone(),
            recurrence: p.recurrence.as_ref().map(SyncRecurrence::from),
            parent: p.parent,
            objective: p.objective.clone(),
            alias: p.alias.clone(),
            is_sequential: p.is_sequential,
            duration: p.duration,
            depends_on: p.depends_on.clone(),
            acts: p.acts.iter().map(SyncPlannedAct::from).collect(),
        }
    }
}

impl From<&PlannedAct> for SyncPlannedAct {
    fn from(a: &PlannedAct) -> Self {
        SyncPlannedAct {
            id: a.id,
            plan_id: a.plan_id,
            phase: SyncActPhase::from(a.phase),
            scheduled_at: a.scheduled_at,
            duration: a.duration,
            completed_at: a.completed_at,
            created_at: a.created_at,
        }
    }
}

impl From<ActPhase> for SyncActPhase {
    fn from(p: ActPhase) -> Self {
        match p {
            ActPhase::NotStarted => SyncActPhase::NotStarted,
            ActPhase::InProgress => SyncActPhase::InProgress,
            ActPhase::Completed => SyncActPhase::Completed,
            ActPhase::Blocked => SyncActPhase::Blocked,
            ActPhase::Cancelled => SyncActPhase::Cancelled,
        }
    }
}

impl From<&Recurrence> for SyncRecurrence {
    fn from(r: &Recurrence) -> Self {
        SyncRecurrence {
            frequency: r.frequency.clone(),
            interval: r.interval,
            count: r.count,
            until: r.until.clone(),
            by_second: r.by_second.clone(),
            by_minute: r.by_minute.clone(),
            by_hour: r.by_hour.clone(),
            by_day: r.by_day.clone(),
            by_month_day: r.by_month_day.clone(),
            by_year_day: r.by_year_day.clone(),
            by_week_no: r.by_week_no.clone(),
            by_month: r.by_month.clone(),
            by_set_pos: r.by_set_pos.clone(),
            week_start: r.week_start.clone(),
        }
    }
}

// ============================================================================
// CRDT → Domain
// ============================================================================

impl From<&SyncModel> for DomainModel {
    fn from(sm: &SyncModel) -> Self {
        DomainModel {
            objectives: sm.objectives.iter().map(Objective::from).collect(),
            charters: sm.charters.iter().map(Charter::from).collect(),
        }
    }
}

impl From<&SyncObjective> for Objective {
    fn from(o: &SyncObjective) -> Self {
        Objective {
            id: o.id,
            title: o.title.clone(),
            description: o.description.clone(),
            alias: o.alias.clone(),
            parent: o.parent.clone(),
            metrics: o
                .metrics
                .as_ref()
                .map(|ms| ms.iter().map(Metric::from).collect()),
        }
    }
}

impl From<&SyncMetric> for Metric {
    fn from(m: &SyncMetric) -> Self {
        Metric {
            name: m.name.clone(),
            description: m.description.clone(),
            target: m.target.clone(),
            review_date: m.review_date.clone(),
        }
    }
}

impl From<&SyncReference> for Reference {
    fn from(r: &SyncReference) -> Self {
        match r {
            SyncReference::UUID(id) => Reference::UUID(*id),
            SyncReference::Prefix(s) => Reference::Prefix(s.clone()),
            SyncReference::Name(s) => Reference::Name(s.clone()),
            SyncReference::Alias(s) => Reference::Alias(s.clone()),
        }
    }
}

impl From<&SyncCharter> for Charter {
    fn from(c: &SyncCharter) -> Self {
        Charter {
            id: c.id,
            title: c.title.clone(),
            description: c.description.clone(),
            alias: c.alias.clone(),
            parent: c.parent.clone(),
            objectives: c.objectives.clone(),
            plans: c.plans.iter().map(Plan::from).collect(),
        }
    }
}

impl From<&SyncPlan> for Plan {
    fn from(p: &SyncPlan) -> Self {
        Plan {
            id: p.id,
            name: p.name.clone(),
            description: p.description.clone(),
            priority: p.priority,
            contexts: p.contexts.clone(),
            recurrence: p.recurrence.as_ref().map(Recurrence::from),
            parent: p.parent,
            objective: p.objective.clone(),
            alias: p.alias.clone(),
            is_sequential: p.is_sequential,
            duration: p.duration,
            depends_on: p.depends_on.clone(),
            acts: p.acts.iter().map(PlannedAct::from).collect(),
        }
    }
}

impl From<&SyncPlannedAct> for PlannedAct {
    fn from(a: &SyncPlannedAct) -> Self {
        PlannedAct {
            id: a.id,
            plan_id: a.plan_id,
            phase: ActPhase::from(a.phase),
            scheduled_at: a.scheduled_at,
            duration: a.duration,
            completed_at: a.completed_at,
            created_at: a.created_at,
        }
    }
}

impl From<SyncActPhase> for ActPhase {
    fn from(p: SyncActPhase) -> Self {
        match p {
            SyncActPhase::NotStarted => ActPhase::NotStarted,
            SyncActPhase::InProgress => ActPhase::InProgress,
            SyncActPhase::Completed => ActPhase::Completed,
            SyncActPhase::Blocked => ActPhase::Blocked,
            SyncActPhase::Cancelled => ActPhase::Cancelled,
        }
    }
}

impl From<&SyncRecurrence> for Recurrence {
    fn from(r: &SyncRecurrence) -> Self {
        Recurrence {
            frequency: r.frequency.clone(),
            interval: r.interval,
            count: r.count,
            until: r.until.clone(),
            by_second: r.by_second.clone(),
            by_minute: r.by_minute.clone(),
            by_hour: r.by_hour.clone(),
            by_day: r.by_day.clone(),
            by_month_day: r.by_month_day.clone(),
            by_year_day: r.by_year_day.clone(),
            by_week_no: r.by_week_no.clone(),
            by_month: r.by_month.clone(),
            by_set_pos: r.by_set_pos.clone(),
            week_start: r.week_start.clone(),
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::actions::Action;
    use uuid::Uuid;

    #[test]
    fn test_domain_to_sync_roundtrip_empty() {
        let dm = DomainModel::new();
        let sync: SyncModel = SyncModel::from(&dm);
        let back: DomainModel = DomainModel::from(&sync);

        assert_eq!(back.all_plans().len(), 0);
        assert_eq!(back.all_acts().len(), 0);
    }

    #[test]
    fn test_domain_to_sync_roundtrip_with_actions() {
        let mut action = Action::new("Test task");
        action.priority = Some(3);
        action.description = Some("A description".to_string());
        action.context_list = Some(vec!["home".to_string(), "computer".to_string()]);

        let dm = DomainModel::from_actions(&vec![action]);
        let sync: SyncModel = SyncModel::from(&dm);
        let back: DomainModel = DomainModel::from(&sync);

        assert_eq!(back.all_plans().len(), 1);
        assert_eq!(back.all_acts().len(), 1);

        let orig_plan = &dm.all_plans()[0];
        let round_plan = &back.all_plans()[0];

        assert_eq!(round_plan.id, orig_plan.id);
        assert_eq!(round_plan.name, orig_plan.name);
        assert_eq!(round_plan.priority, orig_plan.priority);
        assert_eq!(round_plan.description, orig_plan.description);
        assert_eq!(round_plan.contexts, orig_plan.contexts);
    }

    #[test]
    fn test_act_phase_roundtrip() {
        let phases = vec![
            ActPhase::NotStarted,
            ActPhase::InProgress,
            ActPhase::Completed,
            ActPhase::Blocked,
            ActPhase::Cancelled,
        ];

        for phase in phases {
            let sync_phase = SyncActPhase::from(phase);
            let back = ActPhase::from(sync_phase);
            assert_eq!(back, phase);
        }
    }

    #[test]
    fn test_objective_roundtrip() {
        let obj = Objective {
            id: Uuid::new_v4(),
            title: Some("Health".to_string()),
            description: Some("Stay fit".to_string()),
            alias: Some("health".to_string()),
            parent: None,
            metrics: Some(vec![Metric {
                name: "Steps".to_string(),
                description: Some("Daily steps".to_string()),
                target: Some("10000".to_string()),
                review_date: None,
            }]),
        };

        let sync_obj = SyncObjective::from(&obj);
        let back = Objective::from(&sync_obj);

        assert_eq!(back.id, obj.id);
        assert_eq!(back.title, obj.title);
        assert_eq!(back.metrics.as_ref().unwrap().len(), 1);
        assert_eq!(back.metrics.as_ref().unwrap()[0].name, "Steps");
    }

    #[test]
    fn test_charter_with_plans_roundtrip() {
        let action = Action::new("Charter task");
        let dm = DomainModel::from_actions(&vec![action]);

        // The DomainModel wraps things in a synthetic charter
        let sync: SyncModel = SyncModel::from(&dm);
        let back: DomainModel = DomainModel::from(&sync);

        assert_eq!(back.charters.len(), dm.charters.len());
        for (orig_c, round_c) in dm.charters.iter().zip(back.charters.iter()) {
            assert_eq!(round_c.id, orig_c.id);
            assert_eq!(round_c.title, orig_c.title);
            assert_eq!(round_c.plans.len(), orig_c.plans.len());
        }
    }
}
