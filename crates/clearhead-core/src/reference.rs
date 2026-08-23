//! Cross-domain reference resolution.
//!
//! Resolves human-readable reference strings — aliases, short UUID prefixes, full
//! UUIDs, and path-style `charter/plan` notation — to typed [`ReferenceTarget`]s.
//!
//! # Reference Syntax
//!
//! | Form | Example | Resolves to |
//! |------|---------|-------------|
//! | Full UUID | `019de698-0eb4-7ed1-b763-999f7a22282a` | Any target type |
//! | Short prefix (≥4 hex chars) | `019de698` | Any target type |
//! | Alias | `staging-deploy` | Charter or Action |
//! | Path | `work/feature` | Charter → sub-entity |
//! | Prefixed | `c:work`, `p:11223344`, `a:019de698` | Scoped to type |
//!
//! Unscoped references apply identity precedence across all entity types:
//! full UUID, then short UUID, then alias. Type prefixes restrict the candidate
//! set when the strongest tier remains ambiguous. Plans have no alias.

use crate::domain::{Action, Charter, DomainModel, Plan};
use crate::workspace::MarkdownCharter;
use std::fmt;
use uuid::Uuid;

/// The resolved target of a reference lookup.
///
/// Carries the UUID of the matched entity and its type, so callers can
/// dispatch to the appropriate domain object without a second search.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReferenceTarget {
    /// A resolved [`Charter`] UUID.
    Charter(Uuid),
    /// A resolved [`Plan`] UUID.
    Plan(Uuid),
    /// A resolved [`Action`] UUID.
    Action(Uuid),
}

/// Controls how alias segments are matched during resolution.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MatchMode {
    /// Case-insensitive exact string match against an alias.
    Exact,
}

/// The canonical way an entity matched a reference string.
///
/// Ordering is semantic: UUID identity is stronger than a human-readable alias.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ReferenceMatch {
    /// The input parsed as the entity's complete UUID.
    FullUuid,
    /// The input was an unambiguous-capable UUID prefix of at least four hex digits.
    ShortUuid,
    /// The input exactly matched the entity's alias, ignoring ASCII case.
    Alias,
}

/// A domain entity that participates in canonical reference resolution.
pub trait ReferenceEntity {
    /// Stable entity identity.
    fn reference_id(&self) -> Uuid;
    /// Human-readable alias, when this entity type supports aliases.
    fn reference_alias(&self) -> Option<&str>;
}

impl ReferenceEntity for Charter {
    fn reference_id(&self) -> Uuid {
        self.id
    }

    fn reference_alias(&self) -> Option<&str> {
        self.alias.as_deref()
    }
}

impl ReferenceEntity for MarkdownCharter {
    fn reference_id(&self) -> Uuid {
        self.id
    }

    fn reference_alias(&self) -> Option<&str> {
        self.alias.as_deref()
    }
}

impl ReferenceEntity for Action {
    fn reference_id(&self) -> Uuid {
        self.id
    }

    fn reference_alias(&self) -> Option<&str> {
        self.alias.as_deref()
    }
}

impl ReferenceEntity for Plan {
    fn reference_id(&self) -> Uuid {
        self.id
    }

    fn reference_alias(&self) -> Option<&str> {
        None
    }
}

impl<T: ReferenceEntity + ?Sized> ReferenceEntity for &T {
    fn reference_id(&self) -> Uuid {
        (*self).reference_id()
    }

    fn reference_alias(&self) -> Option<&str> {
        (*self).reference_alias()
    }
}

/// Result of selecting a reference from a candidate collection.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReferenceSelection {
    /// No candidate matched UUID or alias syntax.
    NotFound,
    /// Exactly one candidate matched at the strongest available tier.
    Unique {
        index: usize,
        matched_by: ReferenceMatch,
    },
    /// Multiple candidates matched at the strongest available tier.
    Ambiguous {
        indices: Vec<usize>,
        matched_by: ReferenceMatch,
    },
}

/// Select one entity using canonical UUID/alias precedence and ambiguity rules.
pub fn select_reference<T: ReferenceEntity>(items: &[T], input: &str) -> ReferenceSelection {
    select_reference_where(items, input, |_| true)
}

/// Select one entity among candidates accepted by `predicate`.
///
/// Full UUID beats short UUID, which beats alias. Multiple matches at the
/// strongest tier are returned as [`ReferenceSelection::Ambiguous`] rather than
/// silently choosing collection order.
pub fn select_reference_where<T: ReferenceEntity>(
    items: &[T],
    input: &str,
    predicate: impl Fn(&T) -> bool,
) -> ReferenceSelection {
    let mut matches: Vec<(usize, ReferenceMatch)> = items
        .iter()
        .enumerate()
        .filter(|(_, item)| predicate(item))
        .filter_map(|(index, item)| {
            match_entity_reference(item.reference_id(), item.reference_alias(), input)
                .map(|matched_by| (index, matched_by))
        })
        .collect();

    let Some(strongest) = matches.iter().map(|(_, matched_by)| *matched_by).min() else {
        return ReferenceSelection::NotFound;
    };
    matches.retain(|(_, matched_by)| *matched_by == strongest);

    if matches.len() == 1 {
        ReferenceSelection::Unique {
            index: matches[0].0,
            matched_by: strongest,
        }
    } else {
        ReferenceSelection::Ambiguous {
            indices: matches.into_iter().map(|(index, _)| index).collect(),
            matched_by: strongest,
        }
    }
}

/// Classify a UUID reference according to the workspace reference syntax.
///
/// Full UUIDs (including forms accepted by [`Uuid::parse_str`]) and short UUID
/// prefixes are supported. Prefixes contain at least four hexadecimal digits;
/// hyphens are ignored so a prefix copied from a canonical UUID remains valid
/// beyond the first group.
pub fn match_uuid_reference(id: Uuid, input: &str) -> Option<ReferenceMatch> {
    let input = input.trim();
    if let Ok(parsed) = Uuid::parse_str(input) {
        return (parsed == id).then_some(ReferenceMatch::FullUuid);
    }

    if !input.chars().all(|c| c.is_ascii_hexdigit() || c == '-') {
        return None;
    }
    let compact: String = input.chars().filter(|c| *c != '-').collect();
    if compact.len() < 4 {
        return None;
    }

    id.simple()
        .to_string()
        .starts_with(&compact.to_ascii_lowercase())
        .then_some(ReferenceMatch::ShortUuid)
}

/// Classify a reference against an entity UUID and optional alias.
///
/// A UUID-shaped input is identity-only: if it does not identify `id`, it does
/// not fall through and accidentally match an alias. Aliases are exact and
/// case-insensitive; names and titles are deliberately outside reference syntax.
pub fn match_entity_reference(
    id: Uuid,
    alias: Option<&str>,
    input: &str,
) -> Option<ReferenceMatch> {
    let input = input.trim();
    if let Some(kind) = match_uuid_reference(id, input) {
        return Some(kind);
    }
    if Uuid::parse_str(input).is_ok() {
        return None;
    }

    alias
        .filter(|candidate| candidate.eq_ignore_ascii_case(input))
        .map(|_| ReferenceMatch::Alias)
}

/// Options controlling reference resolution behaviour.
#[derive(Debug, Clone, Copy)]
pub struct ReferenceOptions {
    /// When `true`, recognise `c:`, `p:`, and `a:` type prefixes.
    /// Disable if the input is known to be a plain alias or UUID.
    pub allow_prefixes: bool,
    /// How alias segments are compared. Currently only [`MatchMode::Exact`].
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

/// Stable semantic classification for reference resolution failures.
#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReferenceErrorKind {
    /// The supplied reference contained no text.
    Empty,
    /// The supplied reference used malformed prefix or path syntax.
    InvalidSyntax,
    /// No entity matched the syntactically valid reference.
    NotFound,
    /// More than one entity matched at the strongest reference tier.
    Ambiguous,
    /// A type-prefixed path resolved to a different entity type.
    TypeMismatch,
}

/// A typed reference failure with a stable kind and contextual diagnostic.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReferenceError {
    kind: ReferenceErrorKind,
    message: String,
}

impl ReferenceError {
    fn with_kind(kind: ReferenceErrorKind, message: impl Into<String>) -> Self {
        Self {
            kind,
            message: message.into(),
        }
    }

    fn empty() -> Self {
        Self::with_kind(ReferenceErrorKind::Empty, "Reference cannot be empty")
    }

    fn invalid_syntax(message: impl Into<String>) -> Self {
        Self::with_kind(ReferenceErrorKind::InvalidSyntax, message)
    }

    fn not_found(message: impl Into<String>) -> Self {
        Self::with_kind(ReferenceErrorKind::NotFound, message)
    }

    fn ambiguous(message: impl Into<String>) -> Self {
        Self::with_kind(ReferenceErrorKind::Ambiguous, message)
    }

    fn type_mismatch(message: impl Into<String>) -> Self {
        Self::with_kind(ReferenceErrorKind::TypeMismatch, message)
    }

    /// Return the stable semantic category of this failure.
    pub const fn kind(&self) -> ReferenceErrorKind {
        self.kind
    }

    /// Whether more than one entity matched at the strongest tier.
    pub const fn is_ambiguous(&self) -> bool {
        matches!(self.kind, ReferenceErrorKind::Ambiguous)
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
    Action,
}

#[derive(Debug, Clone, Copy)]
enum Scope<'a> {
    Charter(&'a Charter),
    Plan(&'a Charter, &'a Plan),
    Action(&'a Charter, &'a Action),
}

#[derive(Debug, Clone, Copy)]
enum ScopedCandidate<'a> {
    Charter(&'a Charter),
    Plan(&'a Plan),
    Action(&'a Action),
}

impl ReferenceEntity for ScopedCandidate<'_> {
    fn reference_id(&self) -> Uuid {
        match self {
            Self::Charter(charter) => charter.id,
            Self::Plan(plan) => plan.id,
            Self::Action(action) => action.id,
        }
    }

    fn reference_alias(&self) -> Option<&str> {
        match self {
            Self::Charter(charter) => charter.alias.as_deref(),
            Self::Plan(_) => None,
            Self::Action(action) => action.alias.as_deref(),
        }
    }
}

/// Resolve a reference string to a typed [`ReferenceTarget`] within `model`.
///
/// Accepts full UUIDs, short prefixes of at least four hex digits, aliases, and path-style
/// `charter/plan` strings. Use `options` to control prefix handling and
/// match mode.
///
/// # Errors
///
/// Returns [`ReferenceError`] when the reference is empty, matches nothing,
/// matches ambiguously, or when a prefixed reference resolves to the wrong
/// target type.
///
/// # Examples
///
/// ```
/// use clearhead_core::{resolve_reference, ReferenceOptions, ReferenceTarget, DomainModel};
///
/// let model = DomainModel::new(); // empty model
/// let result = resolve_reference(&model, "nonexistent", &ReferenceOptions::default());
/// assert!(result.is_err());
/// ```
pub fn resolve_reference(
    model: &DomainModel,
    input: &str,
    options: &ReferenceOptions,
) -> Result<ReferenceTarget, ReferenceError> {
    let trimmed = input.trim();
    if trimmed.is_empty() {
        return Err(ReferenceError::empty());
    }

    let (prefix, path) = parse_prefix(trimmed, options.allow_prefixes)?;
    let segments = split_segments(path)?;

    match prefix {
        Some(Prefix::Charter) => {
            if segments.len() == 1 {
                return resolve_charter_global(model, segments[0]);
            }
            let target = resolve_path(model, &segments)?;
            match target {
                ReferenceTarget::Charter(_) => Ok(target),
                _ => Err(ReferenceError::type_mismatch(
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
                _ => Err(ReferenceError::type_mismatch(
                    "Reference resolved to a non-plan target; use a Plan UUID",
                )),
            }
        }
        Some(Prefix::Action) => {
            if segments.len() == 1 {
                return resolve_action_global(model, segments[0]);
            }
            let target = resolve_path(model, &segments)?;
            match target {
                ReferenceTarget::Action(_) => Ok(target),
                _ => Err(ReferenceError::type_mismatch(
                    "Reference resolved to a non-action target; use an action alias or UUID",
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

/// Return a [`DomainModel`] containing only the specified charter and,
/// when `recursive` is `true`, all of its descendants.
///
/// Objectives are not included in the filtered result.
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
            for child in model.charters.iter().filter(|c| c.is_child_of(parent)) {
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

/// Return a [`DomainModel`] scoped to a single plan and its owning charter.
///
/// The returned charter contains only the matched plan; all other plans and
/// actions in that charter are excluded. Returns an empty model if the plan
/// is not found.
pub fn filter_model_for_plan(model: &DomainModel, plan_id: Uuid) -> DomainModel {
    for charter in &model.charters {
        if let Some(plan) = charter.plans.iter().find(|p| p.id == plan_id) {
            let mut charter_copy = charter.clone();
            charter_copy.plans = vec![plan.clone()];
            charter_copy.actions.clear();
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

/// Return a [`DomainModel`] scoped to a single action and its owning charter.
///
/// The returned charter contains only the matched action. If the action has
/// an associated plan that plan is also included; other plans are excluded.
/// Returns an empty model if the action is not found.
pub fn filter_model_for_action(model: &DomainModel, action_id: Uuid) -> DomainModel {
    for charter in &model.charters {
        if let Some(action) = charter.actions.iter().find(|a| a.id == action_id) {
            let mut charter_copy = charter.clone();
            charter_copy.actions = vec![action.clone()];
            if let Some(plan_id) = action.plan_id {
                charter_copy.plans.retain(|p| p.id == plan_id);
            } else {
                charter_copy.plans.clear();
            }
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
        "a:" => Some(Prefix::Action),
        _ => None,
    };

    if let Some(found) = prefix {
        let rest = input[2..].trim();
        if rest.is_empty() {
            return Err(ReferenceError::invalid_syntax(
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
        return Err(ReferenceError::invalid_syntax("Reference path is empty"));
    }
    Ok(segments)
}

fn resolve_unscoped_single(
    model: &DomainModel,
    segment: &str,
) -> Result<ReferenceTarget, ReferenceError> {
    let mut candidates: Vec<ScopedCandidate<'_>> = model
        .charters
        .iter()
        .map(ScopedCandidate::Charter)
        .collect();
    candidates.extend(
        model
            .charters
            .iter()
            .flat_map(|charter| &charter.plans)
            .map(ScopedCandidate::Plan),
    );
    candidates.extend(
        model
            .charters
            .iter()
            .flat_map(|charter| &charter.actions)
            .map(ScopedCandidate::Action),
    );

    match select_reference(&candidates, segment) {
        ReferenceSelection::NotFound => Err(ReferenceError::not_found(format!(
            "No entity matches reference '{}'",
            segment
        ))),
        ReferenceSelection::Unique { index, .. } => match candidates[index] {
            ScopedCandidate::Charter(charter) => Ok(ReferenceTarget::Charter(charter.id)),
            ScopedCandidate::Plan(plan) => Ok(ReferenceTarget::Plan(plan.id)),
            ScopedCandidate::Action(action) => Ok(ReferenceTarget::Action(action.id)),
        },
        ReferenceSelection::Ambiguous { .. } => Err(ReferenceError::ambiguous(format!(
            "Ambiguous reference '{}'; use a type prefix, path, or longer UUID prefix",
            segment
        ))),
    }
}

fn resolve_charter_global(
    model: &DomainModel,
    segment: &str,
) -> Result<ReferenceTarget, ReferenceError> {
    match select_reference(&model.charters, segment) {
        ReferenceSelection::NotFound => Err(ReferenceError::not_found(format!(
            "No charter matches reference '{}'",
            segment
        ))),
        ReferenceSelection::Unique { index, .. } => {
            Ok(ReferenceTarget::Charter(model.charters[index].id))
        }
        ReferenceSelection::Ambiguous { .. } => Err(ReferenceError::ambiguous(format!(
            "Ambiguous charter reference '{}'; use c:<alias> or c:<uuid>",
            segment
        ))),
    }
}

fn resolve_plan_global(
    model: &DomainModel,
    segment: &str,
) -> Result<ReferenceTarget, ReferenceError> {
    let candidates: Vec<&Plan> = model
        .charters
        .iter()
        .flat_map(|charter| &charter.plans)
        .collect();

    match select_reference(&candidates, segment) {
        ReferenceSelection::NotFound => Err(ReferenceError::not_found(format!(
            "No plan matches reference '{}'",
            segment
        ))),
        ReferenceSelection::Unique { index, .. } => Ok(ReferenceTarget::Plan(candidates[index].id)),
        ReferenceSelection::Ambiguous { .. } => Err(ReferenceError::ambiguous(format!(
            "Ambiguous plan reference '{}'; use a longer UUID prefix",
            segment
        ))),
    }
}

fn resolve_action_global(
    model: &DomainModel,
    segment: &str,
) -> Result<ReferenceTarget, ReferenceError> {
    let candidates: Vec<&Action> = model
        .charters
        .iter()
        .flat_map(|charter| &charter.actions)
        .collect();

    match select_reference(&candidates, segment) {
        ReferenceSelection::NotFound => Err(ReferenceError::not_found(format!(
            "No action matches reference '{}'",
            segment
        ))),
        ReferenceSelection::Unique { index, .. } => {
            Ok(ReferenceTarget::Action(candidates[index].id))
        }
        ReferenceSelection::Ambiguous { .. } => Err(ReferenceError::ambiguous(format!(
            "Ambiguous action reference '{}'; use a path or longer UUID prefix",
            segment
        ))),
    }
}

fn resolve_path(model: &DomainModel, segments: &[&str]) -> Result<ReferenceTarget, ReferenceError> {
    let first = segments
        .first()
        .ok_or_else(|| ReferenceError::invalid_syntax("Reference path is empty"))?;

    let mut scope =
        match select_reference_where(&model.charters, first, |charter| charter.is_root()) {
            ReferenceSelection::NotFound => {
                return Err(ReferenceError::not_found(format!(
                    "No charter matches root reference '{}'",
                    first
                )));
            }
            ReferenceSelection::Unique { index, .. } => Scope::Charter(&model.charters[index]),
            ReferenceSelection::Ambiguous { .. } => {
                return Err(ReferenceError::ambiguous(format!(
                    "Ambiguous root charter reference '{}'; use c:<alias> or c:<uuid>",
                    first
                )));
            }
        };

    for segment in &segments[1..] {
        scope = match scope {
            Scope::Charter(charter) => {
                let mut candidates: Vec<ScopedCandidate<'_>> = model
                    .charters
                    .iter()
                    .filter(|candidate| candidate.is_child_of(charter))
                    .map(ScopedCandidate::Charter)
                    .collect();
                candidates.extend(charter.plans.iter().map(ScopedCandidate::Plan));
                candidates.extend(
                    charter
                        .actions
                        .iter()
                        .filter(|action| action.parent_id.is_none())
                        .map(ScopedCandidate::Action),
                );

                match select_reference(&candidates, segment) {
                    ReferenceSelection::NotFound => {
                        return Err(ReferenceError::not_found(format!(
                            "No match for '{}' under charter '{}'",
                            segment, charter.title
                        )));
                    }
                    ReferenceSelection::Unique { index, .. } => match candidates[index] {
                        ScopedCandidate::Charter(child) => Scope::Charter(child),
                        ScopedCandidate::Plan(plan) => Scope::Plan(charter, plan),
                        ScopedCandidate::Action(action) => Scope::Action(charter, action),
                    },
                    ReferenceSelection::Ambiguous { .. } => {
                        return Err(ReferenceError::ambiguous(format!(
                            "Ambiguous reference '{}' under charter '{}'; use a type prefix or UUID",
                            segment, charter.title
                        )));
                    }
                }
            }
            Scope::Plan(charter, plan) => {
                let candidates: Vec<&Action> = charter
                    .actions
                    .iter()
                    .filter(|action| action.plan_id == Some(plan.id))
                    .collect();
                match select_reference(&candidates, segment) {
                    ReferenceSelection::NotFound => {
                        return Err(ReferenceError::not_found(format!(
                            "No match for '{}' under plan '{}'",
                            segment, plan.name
                        )));
                    }
                    ReferenceSelection::Unique { index, .. } => {
                        Scope::Action(charter, candidates[index])
                    }
                    ReferenceSelection::Ambiguous { .. } => {
                        return Err(ReferenceError::ambiguous(format!(
                            "Ambiguous reference '{}' under plan '{}'; use a longer path or UUID",
                            segment, plan.name
                        )));
                    }
                }
            }
            Scope::Action(charter, action) => {
                let candidates: Vec<&Action> = charter
                    .actions
                    .iter()
                    .filter(|candidate| candidate.parent_id == Some(action.id))
                    .collect();
                match select_reference(&candidates, segment) {
                    ReferenceSelection::NotFound => {
                        return Err(ReferenceError::not_found(format!(
                            "No action matches '{}' under action '{}'",
                            segment, action.name
                        )));
                    }
                    ReferenceSelection::Unique { index, .. } => {
                        Scope::Action(charter, candidates[index])
                    }
                    ReferenceSelection::Ambiguous { .. } => {
                        return Err(ReferenceError::ambiguous(format!(
                            "Ambiguous action reference '{}' under action '{}'; use a UUID",
                            segment, action.name
                        )));
                    }
                }
            }
        };
    }

    match scope {
        Scope::Charter(charter) => Ok(ReferenceTarget::Charter(charter.id)),
        Scope::Plan(_, plan) => Ok(ReferenceTarget::Plan(plan.id)),
        Scope::Action(_, action) => Ok(ReferenceTarget::Action(action.id)),
    }
}

/// Resolve a reference across multiple workspaces, returning the first match.
///
/// `workspaces` should be ordered primary-first; the caller controls
/// precedence. Returns the matching workspace name alongside the target.
/// Invalid syntax and ambiguity stop resolution immediately. A type mismatch
/// is returned only if no later workspace resolves the same reference.
pub fn resolve_reference_in_workspaces(
    workspaces: &[(&str, &DomainModel)],
    input: &str,
    options: &ReferenceOptions,
) -> Result<(String, ReferenceTarget), ReferenceError> {
    let trimmed = input.trim();
    if trimmed.is_empty() {
        return Err(ReferenceError::empty());
    }
    let (_, path) = parse_prefix(trimmed, options.allow_prefixes)?;
    split_segments(path)?;

    let mut type_mismatch = None;
    for (name, model) in workspaces {
        match resolve_reference(model, trimmed, options) {
            Ok(target) => return Ok((name.to_string(), target)),
            Err(error) => match error.kind() {
                ReferenceErrorKind::Empty | ReferenceErrorKind::InvalidSyntax => return Err(error),
                ReferenceErrorKind::Ambiguous => return Err(error),
                ReferenceErrorKind::TypeMismatch => {
                    type_mismatch.get_or_insert(error);
                }
                ReferenceErrorKind::NotFound => {}
            },
        }
    }
    if let Some(error) = type_mismatch {
        return Err(error);
    }
    Err(ReferenceError::not_found(format!(
        "No match for '{}' in any workspace",
        trimmed
    )))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_plan(id: Uuid, name: &str) -> Plan {
        Plan {
            id,
            name: name.to_string(),
            ..Default::default()
        }
    }

    fn make_action(id: Uuid, plan_id: Uuid) -> Action {
        Action {
            id,
            name: "action".to_string(),
            plan_id: Some(plan_id),
            ..Default::default()
        }
    }

    fn sample_model() -> DomainModel {
        let charter_id = Uuid::parse_str("12345678-0000-0000-0000-000000000001").unwrap();
        let child_charter_id = Uuid::parse_str("abcdef12-0000-0000-0000-000000000002").unwrap();
        let implicit_charter_id = Uuid::parse_str("abcdef12-0000-0000-0000-000000000099").unwrap();
        let plan_id = Uuid::parse_str("11223344-0000-0000-0000-000000000003").unwrap();
        let subplan_id = Uuid::parse_str("55667788-0000-0000-0000-000000000004").unwrap();
        let action_id = Uuid::parse_str("deadbeef-0000-0000-0000-000000000005").unwrap();

        let plan = make_plan(plan_id, "core");

        let subplan = make_plan(subplan_id, "resolver");

        let charter = Charter {
            id: charter_id,
            title: "Build".to_string(),
            description: None,
            alias: Some("build".to_string()),
            parent: None,
            objectives: None,
            state: None,
            plans: vec![plan, subplan],
            actions: vec![make_action(action_id, plan_id)],
        };

        let child_charter = Charter {
            id: child_charter_id,
            title: "Observability".to_string(),
            description: None,
            alias: Some("obs".to_string()),
            parent: Some("build".to_string()),
            objectives: None,
            state: None,
            plans: vec![],
            actions: vec![],
        };

        let implicit_charter = Charter {
            id: implicit_charter_id,
            title: "".to_string(),
            description: None,
            alias: Some("implicit".to_string()),
            parent: None,
            objectives: None,
            state: None,
            plans: vec![],
            actions: vec![],
        };

        DomainModel {
            objectives: vec![],
            charters: vec![charter, child_charter, implicit_charter],
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
    fn resolves_plan_path_by_uuid() {
        let model = sample_model();
        let plan_id = Uuid::parse_str("11223344-0000-0000-0000-000000000003").unwrap();
        let short = &plan_id.to_string()[..8];
        let path = format!("build/{}", short);
        let target = resolve_reference(&model, &path, &ReferenceOptions::default()).unwrap();
        assert_eq!(target, ReferenceTarget::Plan(plan_id));
    }

    #[test]
    fn path_selection_applies_uuid_precedence_across_entity_types() {
        let mut model = sample_model();
        model.charters[1].alias = Some("1122".to_string());
        let plan_id = model.charters[0].plans[0].id;

        let target = resolve_reference(&model, "build/1122", &ReferenceOptions::default()).unwrap();
        assert_eq!(target, ReferenceTarget::Plan(plan_id));
    }

    #[test]
    fn unscoped_selection_applies_uuid_precedence_across_entity_types() {
        let mut model = sample_model();
        model.charters[0].alias = Some("dead".to_string());
        let action_id = model.charters[0].actions[0].id;

        let target = resolve_reference(&model, "dead", &ReferenceOptions::default()).unwrap();
        assert_eq!(target, ReferenceTarget::Action(action_id));
    }

    #[test]
    fn resolves_act_in_plan_path() {
        let model = sample_model();
        let plan_id = Uuid::parse_str("11223344-0000-0000-0000-000000000003").unwrap();
        let action_id = Uuid::parse_str("deadbeef-0000-0000-0000-000000000005").unwrap();
        let plan_short = &plan_id.to_string()[..8];
        let action_short = &action_id.to_string()[..8];
        let path = format!("build/{}/{}", plan_short, action_short);
        let target = resolve_reference(&model, &path, &ReferenceOptions::default()).unwrap();
        assert_eq!(target, ReferenceTarget::Action(action_id));
    }

    #[test]
    fn resolves_plan_prefix_globally_by_uuid() {
        let model = sample_model();
        let plan_id = Uuid::parse_str("11223344-0000-0000-0000-000000000003").unwrap();
        let short = &plan_id.to_string()[..8];
        let input = format!("p:{}", short);
        let target = resolve_reference(&model, &input, &ReferenceOptions::default()).unwrap();
        assert_eq!(target, ReferenceTarget::Plan(plan_id));
    }

    #[test]
    fn rejects_missing_prefix_value() {
        let model = sample_model();
        let err = resolve_reference(&model, "c:", &ReferenceOptions::default()).unwrap_err();
        assert_eq!(err.kind(), ReferenceErrorKind::InvalidSyntax);
        assert!(err.to_string().contains("prefix"));
    }

    #[test]
    fn rejects_empty_reference_with_typed_kind() {
        let err =
            resolve_reference(&DomainModel::new(), "  ", &ReferenceOptions::default()).unwrap_err();
        assert_eq!(err.kind(), ReferenceErrorKind::Empty);
        assert!(!err.is_ambiguous());
        assert_eq!(err.to_string(), "Reference cannot be empty");
    }

    #[test]
    fn finds_implicit_charter() {
        let model = sample_model();
        let target = resolve_reference(&model, "implicit", &ReferenceOptions::default()).unwrap();
        assert_eq!(
            target,
            ReferenceTarget::Charter(
                Uuid::parse_str("abcdef12-0000-0000-0000-000000000099").unwrap()
            )
        );
    }

    #[test]
    fn charter_prefix_resolves_a_child_alias_globally() {
        let model = sample_model();
        let target = resolve_reference(&model, "c:obs", &ReferenceOptions::default()).unwrap();
        assert_eq!(target, ReferenceTarget::Charter(model.charters[1].id));
    }

    #[test]
    fn multi_workspace_returns_first_match() {
        let model = sample_model();
        let empty = DomainModel {
            objectives: vec![],
            charters: vec![],
        };
        let workspaces = [("secondary", &empty), ("primary", &model)];
        let (ws, target) =
            resolve_reference_in_workspaces(&workspaces, "build", &ReferenceOptions::default())
                .unwrap();
        assert_eq!(ws, "primary");
        assert_eq!(
            target,
            ReferenceTarget::Charter(
                Uuid::parse_str("12345678-0000-0000-0000-000000000001").unwrap()
            )
        );
    }

    #[test]
    fn multi_workspace_error_when_none_match() {
        let empty = DomainModel {
            objectives: vec![],
            charters: vec![],
        };
        let workspaces = [("a", &empty), ("b", &empty)];
        let err =
            resolve_reference_in_workspaces(&workspaces, "missing", &ReferenceOptions::default())
                .unwrap_err();
        assert_eq!(err.kind(), ReferenceErrorKind::NotFound);
        assert!(err.to_string().contains("missing"));
    }

    #[test]
    fn multi_workspace_preserves_typed_input_and_target_failures() {
        let no_workspaces = [];
        let empty =
            resolve_reference_in_workspaces(&no_workspaces, " ", &ReferenceOptions::default())
                .unwrap_err();
        assert_eq!(empty.kind(), ReferenceErrorKind::Empty);

        let invalid =
            resolve_reference_in_workspaces(&no_workspaces, "c:", &ReferenceOptions::default())
                .unwrap_err();
        assert_eq!(invalid.kind(), ReferenceErrorKind::InvalidSyntax);

        let model = sample_model();
        let workspaces = [("primary", &model)];
        let mismatch = resolve_reference_in_workspaces(
            &workspaces,
            "p:build/dead",
            &ReferenceOptions::default(),
        )
        .unwrap_err();
        assert_eq!(mismatch.kind(), ReferenceErrorKind::TypeMismatch);

        let mut valid = sample_model();
        let plan_id = Uuid::parse_str("deadbeef-0000-0000-0000-000000000005").unwrap();
        valid.charters[0].actions.clear();
        valid.charters[0]
            .plans
            .push(make_plan(plan_id, "matching-plan"));
        let workspaces = [("mismatch", &model), ("valid", &valid)];
        let (workspace, target) = resolve_reference_in_workspaces(
            &workspaces,
            "p:build/dead",
            &ReferenceOptions::default(),
        )
        .unwrap();
        assert_eq!(workspace, "valid");
        assert_eq!(target, ReferenceTarget::Plan(plan_id));
    }

    #[test]
    fn ambiguity_does_not_fall_through_to_a_different_entity_type() {
        let mut model = sample_model();
        let mut duplicate = model.charters[0].clone();
        duplicate.id = Uuid::parse_str("12345678-ffff-ffff-ffff-ffffffffffff").unwrap();
        model.charters.push(duplicate);

        let err = resolve_reference(&model, "build", &ReferenceOptions::default()).unwrap_err();
        assert!(err.to_string().contains("Ambiguous reference"));
    }

    #[test]
    fn ambiguity_in_an_earlier_workspace_does_not_fall_through_to_a_later_one() {
        let mut ambiguous = sample_model();
        let mut duplicate = ambiguous.charters[0].clone();
        duplicate.id = Uuid::parse_str("12345678-ffff-ffff-ffff-ffffffffffff").unwrap();
        ambiguous.charters.push(duplicate);
        let valid = sample_model();
        let workspaces = [("ambiguous", &ambiguous), ("valid", &valid)];

        let err =
            resolve_reference_in_workspaces(&workspaces, "build", &ReferenceOptions::default())
                .unwrap_err();
        assert_eq!(err.kind(), ReferenceErrorKind::Ambiguous);
        assert!(err.is_ambiguous());
        assert!(err.to_string().contains("Ambiguous reference"));
    }

    #[test]
    fn short_uuid_prefix_longer_than_eight_resolves() {
        let model = sample_model();
        let action_id = Uuid::parse_str("deadbeef-0000-0000-0000-000000000005").unwrap();
        let long_prefix = &action_id.to_string().replace('-', "")[..12];
        let target = resolve_reference(
            &model,
            &format!("a:{}", long_prefix),
            &ReferenceOptions::default(),
        )
        .unwrap();
        assert_eq!(target, ReferenceTarget::Action(action_id));
    }

    #[test]
    fn canonical_uuid_matching_accepts_four_or_more_hex_digits_and_hyphens() {
        let id = Uuid::parse_str("deadbeef-1234-5678-9abc-000000000005").unwrap();
        assert_eq!(
            match_uuid_reference(id, "dead"),
            Some(ReferenceMatch::ShortUuid)
        );
        assert_eq!(
            match_uuid_reference(id, "DEADBEEF-1234"),
            Some(ReferenceMatch::ShortUuid)
        );
        assert_eq!(match_uuid_reference(id, "dea"), None);
    }

    #[test]
    fn canonical_alias_matching_is_exact_and_case_insensitive() {
        let id = Uuid::parse_str("deadbeef-1234-5678-9abc-000000000005").unwrap();
        assert_eq!(
            match_entity_reference(id, Some("Deploy"), "DEPLOY"),
            Some(ReferenceMatch::Alias)
        );
        assert_eq!(match_entity_reference(id, Some("Deploy"), "depl"), None);
    }

    #[test]
    fn selection_reports_ambiguous_short_uuid_prefixes() {
        let actions = vec![
            Action {
                id: Uuid::parse_str("dead0000-0000-0000-0000-000000000001").unwrap(),
                ..Default::default()
            },
            Action {
                id: Uuid::parse_str("deadffff-0000-0000-0000-000000000002").unwrap(),
                ..Default::default()
            },
        ];
        assert_eq!(
            select_reference(&actions, "dead"),
            ReferenceSelection::Ambiguous {
                indices: vec![0, 1],
                matched_by: ReferenceMatch::ShortUuid,
            }
        );
    }

    #[test]
    fn selection_applies_identity_before_alias_across_the_collection() {
        let identity = Uuid::parse_str("deadbeef-0000-0000-0000-000000000001").unwrap();
        let actions = vec![
            Action {
                alias: Some("deadbeef".to_string()),
                ..Default::default()
            },
            Action {
                id: identity,
                ..Default::default()
            },
        ];
        assert_eq!(
            select_reference(&actions, "deadbeef"),
            ReferenceSelection::Unique {
                index: 1,
                matched_by: ReferenceMatch::ShortUuid,
            }
        );
    }

    #[test]
    fn resolves_action_alias() {
        let mut model = sample_model();
        model.charters[0].actions[0].alias = Some("ship".to_string());
        let action_id = model.charters[0].actions[0].id;
        let target = resolve_reference(&model, "a:SHIP", &ReferenceOptions::default()).unwrap();
        assert_eq!(target, ReferenceTarget::Action(action_id));
    }

    #[test]
    fn resolves_path_scoped_action_aliases() {
        let mut model = sample_model();
        let root_id = model.charters[0].actions[0].id;
        model.charters[0].actions[0].alias = Some("deploy".to_string());
        let child_id = Uuid::parse_str("feedface-0000-0000-0000-000000000006").unwrap();
        let mut child = make_action(child_id, model.charters[0].plans[0].id);
        child.parent_id = Some(root_id);
        child.alias = Some("verify".to_string());
        model.charters[0].actions.push(child);

        let target =
            resolve_reference(&model, "build/deploy/verify", &ReferenceOptions::default()).unwrap();
        assert_eq!(target, ReferenceTarget::Action(child_id));
    }
}
