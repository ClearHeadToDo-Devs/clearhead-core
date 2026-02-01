use automerge::ObjId;
use autosurgeon::{Hydrate, HydrateError, Prop, ReadDoc, Reconcile, Reconciler};
use chrono::{DateTime, Local};

/// Helper for reconciling Chrono DateTime as ISO8601 strings in Automerge
pub fn reconcile_date<R: Reconciler>(
    date: &Option<DateTime<Local>>,
    reconciler: R,
) -> Result<(), R::Error> {
    match date {
        Some(d) => d.to_rfc3339().reconcile(reconciler),
        None => None::<String>.reconcile(reconciler),
    }
}

/// Helper for hydrating Chrono DateTime from strings in Automerge
pub fn hydrate_date<D: ReadDoc>(
    doc: &D,
    obj: &ObjId,
    prop: Prop<'_>,
) -> Result<Option<DateTime<Local>>, HydrateError> {
    let val = Option::<String>::hydrate(doc, obj, prop)?;
    match val {
        Some(s) => DateTime::parse_from_rfc3339(&s)
            .map(|dt| Some(dt.with_timezone(&Local)))
            .map_err(|e| {
                HydrateError::unexpected("ISO8601 DateTime string", format!("{}: {}", s, e))
            }),
        None => Ok(None),
    }
}
