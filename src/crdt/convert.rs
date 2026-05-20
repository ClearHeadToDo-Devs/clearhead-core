//! Conversion between domain types and CRDT mirror types.
//!
//! This is the boundary layer that keeps the domain model free of
//! CRDT concerns. All Automerge Reconcile/Hydrate derives live on
//! the `crdt::types` side; domain types remain pure business objects.

use crate::domain::{
    ActionState, Action, Charter, DomainModel, Metric, Objective, Plan, Recurrence, Reference,
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
            acts: c.actions.iter().map(SyncPlannedAct::from).collect(),
        }
    }
}

impl From<&Plan> for SyncPlan {
    fn from(p: &Plan) -> Self {
        SyncPlan {
            id: p.id,
            name: p.name.clone(),
            description: p.description.clone(),
            recurrence: p.recurrence.as_ref().map(SyncRecurrence::from),
            due_recurrence: p.due_recurrence.as_ref().map(SyncRecurrence::from),
        }
    }
}

impl From<&Action> for SyncPlannedAct {
    fn from(a: &Action) -> Self {
        SyncPlannedAct {
            id: a.id,
            plan_id: a.plan_id,
            external_schedule_id: a.external_schedule_id.clone(),
            external_occurrence_key: a.external_occurrence_key.clone(),
            phase: SyncActPhase::from(a.state),
            scheduled_at: a.scheduled_at,
            due_date: a.due_date,
            duration: a.duration,
            completed_at: a.completed_at,
            created_at: a.created_at,
        }
    }
}

impl From<ActionState> for SyncActPhase {
    fn from(p: ActionState) -> Self {
        match p {
            ActionState::NotStarted => SyncActPhase::NotStarted,
            ActionState::InProgress => SyncActPhase::InProgress,
            ActionState::Completed => SyncActPhase::Completed,
            ActionState::BlockedOrAwaiting => SyncActPhase::Blocked,
            ActionState::Cancelled => SyncActPhase::Cancelled,
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
            state: None,
            plans: c.plans.iter().map(Plan::from).collect(),
            actions: c.acts.iter().map(Action::from).collect(),
        }
    }
}

impl From<&SyncPlan> for Plan {
    fn from(p: &SyncPlan) -> Self {
        Plan {
            id: p.id,
            name: p.name.clone(),
            description: p.description.clone(),
            recurrence: p.recurrence.as_ref().map(Recurrence::from),
            due_recurrence: p.due_recurrence.as_ref().map(Recurrence::from),
            ..Default::default()
        }
    }
}

impl From<&SyncPlannedAct> for Action {
    fn from(a: &SyncPlannedAct) -> Self {
        Action {
            id: a.id,
            name: String::new(),
            plan_id: a.plan_id,
            external_schedule_id: a.external_schedule_id.clone(),
            external_occurrence_key: a.external_occurrence_key.clone(),
            state: ActionState::from(a.phase),
            scheduled_at: a.scheduled_at,
            due_date: a.due_date,
            duration: a.duration,
            completed_at: a.completed_at,
            created_at: a.created_at,
            ..Default::default()
        }
    }
}

impl From<SyncActPhase> for ActionState {
    fn from(p: SyncActPhase) -> Self {
        match p {
            SyncActPhase::NotStarted => ActionState::NotStarted,
            SyncActPhase::InProgress => ActionState::InProgress,
            SyncActPhase::Completed => ActionState::Completed,
            SyncActPhase::Blocked => ActionState::BlockedOrAwaiting,
            SyncActPhase::Cancelled => ActionState::Cancelled,
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

// CRDT roundtrip tests removed 2026-04-12 — CRDT layer is deferred (reserved for
// future JS sync server). Tests will be rewritten fresh when that work resumes.
