use clearhead_core::domain::habit_slots::{
    SlotFrame, SlotKey, canonical_slot, occurrence_id, step_id,
};
use uuid::Uuid;

const HABIT: &str = "01951111-cfa6-718d-b303-d7107f4005b4";

#[test]
fn habit_slot_identity_precision_and_offset_vectors() {
    for (frame, source, expected) in [
        (SlotFrame::Date, "20261026", "D:2026-10-26"),
        (SlotFrame::Date, "2026-10-26", "D:2026-10-26"),
        (
            SlotFrame::Floating,
            "20261026T090000",
            "F:2026-10-26T09:00:00",
        ),
        (
            SlotFrame::Floating,
            "2026-10-26T09:00",
            "F:2026-10-26T09:00:00",
        ),
        (
            SlotFrame::Floating,
            "2026-10-26T09:00:00.000",
            "F:2026-10-26T09:00:00",
        ),
        (
            SlotFrame::Floating,
            "2026-10-26T09:00:00.1250",
            "F:2026-10-26T09:00:00.125",
        ),
        (
            SlotFrame::Floating,
            "2026-10-26T09:00:00.1234567891230",
            "F:2026-10-26T09:00:00.123456789123",
        ),
        (
            SlotFrame::UtcOffset,
            "20261026T160000Z",
            "U:2026-10-26T16:00:00Z",
        ),
        (
            SlotFrame::UtcOffset,
            "2026-10-26T09:00:00-07:00",
            "U:2026-10-26T16:00:00Z",
        ),
        (
            SlotFrame::UtcOffset,
            "2026-10-27T01:00:00+09:00",
            "U:2026-10-26T16:00:00Z",
        ),
        (
            SlotFrame::UtcOffset,
            "2026-10-26T16:00Z",
            "U:2026-10-26T16:00:00Z",
        ),
        (
            SlotFrame::UtcOffset,
            "2026-10-26T09:00:00.1250-07:00",
            "U:2026-10-26T16:00:00.125Z",
        ),
    ] {
        let slot = canonical_slot(frame.clone(), source).unwrap();
        assert_eq!(slot.frame, frame);
        assert_eq!(slot.key.as_str(), expected, "{source}");
        assert_eq!(expected.parse::<SlotKey>().unwrap(), slot.key);
    }
}

#[test]
fn habit_slot_identity_valid_and_invalid_key_vectors() {
    for key in [
        "D:2026-10-26",
        "F:2026-10-26T09:00:00",
        "U:2026-10-26T16:00:00Z",
        "F:2026-10-26T09:00:00.125",
        "U:2026-10-26T16:00:00.125Z",
    ] {
        assert_eq!(key.parse::<SlotKey>().unwrap().as_str(), key);
    }
    for key in [
        "F:2026-10-26T",
        "U:2026-10-26T16:00:00ZExtra",
        "U:2026-10-26T16:00:00",
        "F:2026-10-26T09:00:00Z",
        "F:2026-10-26T09:00",
        "U:2026-10-26T16:00:00.120Z",
        "D:2026-02-30",
        "D:20261026",
        "U:2026-10-26T09:00:00-07:00",
        "F:2026-10-26T09:00:00.0",
    ] {
        assert!(key.parse::<SlotKey>().is_err(), "accepted {key}");
    }
    for (frame, source) in [
        (SlotFrame::Date, "2026-02-30"),
        (SlotFrame::Floating, "2026-10-26T09:00:00Z"),
        (SlotFrame::UtcOffset, "2026-10-26T09:00:00"),
        (SlotFrame::Floating, "2026-10-26T25:00:00"),
    ] {
        assert!(canonical_slot(frame, source).is_err(), "accepted {source}");
    }
}

#[test]
fn habit_slot_identity_series_zone_fold_gap_and_moved_original_slot() {
    let frame = SlotFrame::Tzid("America/Los_Angeles".parse().unwrap());
    let habit = Uuid::parse_str(HABIT).unwrap();
    for (original, expected) in [
        ("20261026T090000", "U:2026-10-26T16:00:00Z"),
        ("2026-11-01T01:30:00", "U:2026-11-01T08:30:00Z"),
        ("2026-03-08T02:30:00", "U:2026-03-08T10:30:00Z"),
        ("2026-03-08T02:30:00.1250", "U:2026-03-08T10:30:00.125Z"),
    ] {
        // The caller supplies RECURRENCE-ID, never a moved DTSTART or transport UID.
        let slot = canonical_slot(frame.clone(), original).unwrap();
        assert_eq!(slot.frame, frame);
        assert_eq!(slot.key.as_str(), expected);
        let utc = canonical_slot(SlotFrame::UtcOffset, &expected[2..]).unwrap();
        assert_eq!(
            occurrence_id(habit, &slot.key),
            occurrence_id(habit, &utc.key)
        );
        let moved_dtstart = canonical_slot(frame.clone(), "2026-11-02T15:00:00").unwrap();
        assert_ne!(
            occurrence_id(habit, &slot.key),
            occurrence_id(habit, &moved_dtstart.key)
        );
    }
}

#[test]
fn habit_slot_identity_native_uuid_vectors() {
    let habit = Uuid::parse_str(HABIT).unwrap();
    let key = "D:2026-10-26".parse::<SlotKey>().unwrap();
    let root = occurrence_id(habit, &key);
    assert_eq!(root.to_string(), "8e337bcd-ad84-5158-baec-ac9b81c5ab0f");
    let template = Uuid::parse_str("01951111-CFA6-718D-B303-D7107F4005B6").unwrap();
    assert_eq!(
        step_id(root, template),
        Uuid::parse_str("d055f426-53c2-5172-a7a3-88e183fb92a6").unwrap()
    );
    assert_ne!(step_id(root, template), root);
    assert_ne!(occurrence_id(Uuid::nil(), &key), root);
    assert_ne!(step_id(root, Uuid::nil()), step_id(root, template));
}
