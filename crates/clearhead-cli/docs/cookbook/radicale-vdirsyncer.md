# Compose ClearHead with Radicale and vdirsyncer

This recipe publishes ClearHead Actions and recurring Plans to CalDAV while keeping the integration server-agnostic.

- ClearHead owns `.actions` files and reads/writes a configured iCalendar vdir.
- vdirsyncer transports vdir resources through CalDAV.
- Radicale owns its private server storage and serves CalDAV clients.
- Calendar or task clients communicate with Radicale, not with ClearHead files.

The normative identity, field mapping, and reconciliation rules are defined by the [iCalendar VTODO projection specification](https://github.com/ClearHeadToDo-Devs/specifications/blob/master/ics_schedule_spec.md). This document only describes one operational composition.

## Topology

```text
calendar/task clients
        ↕ CalDAV
      Radicale
        ↕ CalDAV
     vdirsyncer
        ↕ filesystem
 configured plans vdir
        ↕
clearhead sync calendar
        ↕
     .actions files
```

Sharing a host does not collapse these boundaries. Even when Radicale, vdirsyncer, and ClearHead run on one homelab server, ClearHead must not edit Radicale's internal storage. Doing so bypasses CalDAV preconditions, ETags, locking, and server metadata.

## Prerequisites

Install and configure:

- `clearhead` with a user workspace;
- `vdirsyncer`;
- a reachable Radicale account;
- a VTODO-capable calendar or task client.

Some calendar applications synchronize CalDAV events but do not display tasks. Use a client that explicitly supports VTODO when testing the user interface.

Back up the ClearHead plans vdir and the dedicated Radicale collections before changing an existing deployment.

## Choose the filesystem boundary

The default user-workspace vdir is:

```text
~/.local/share/clearhead/plans/
```

Its immediate child directories are charter-scoped collections:

```text
plans/
├── financial/
│   └── <resource>.ics
└── reflections/
    └── <resource>.ics
```

ClearHead calculates this collection topology from the workspace's charter anchors. A new resource in a known collection is expected; an unknown immediate child directory is quarantined rather than silently creating a charter. Inspect it with `clearhead doctor`. `clearhead doctor --fix --dry-run` previews removal, and `clearhead doctor --fix` removes it locally with a warning because vdirsyncer may propagate that deletion to the CalDAV server.

To use a different location, set `plan_path` in `~/.config/clearhead/config.json`:

```json
{
  "plan_path": "~/.local/share/clearhead/plans"
}
```

Confirm the resolved path before connecting a transport:

```sh
clearhead debug
```

## Configure vdirsyncer

The following is a starting point, not a substitute for the vdirsyncer and Radicale documentation. Put it in the vdirsyncer configuration file for the service account running the synchronization.

```ini
[general]
status_path = "~/.local/share/vdirsyncer/status/"

[pair clearhead]
a = "clearhead_local"
b = "clearhead_radicale"
collections = ["from a", "from b"]
metadata = ["displayname"]

[storage clearhead_local]
type = "filesystem"
path = "~/.local/share/clearhead/plans/"
fileext = ".ics"

[storage clearhead_radicale]
type = "caldav"
url = "https://calendar.example.test/USERNAME/"
username = "USERNAME"
password.fetch = ["command", "pass", "radicale/USERNAME"]
```

This example uses `pass`; initialize its password store and insert the matching Radicale credential before discovery:

```sh
pass insert radicale/USERNAME
```

Use a dedicated Radicale account or collection namespace for ClearHead during the initial deployment. Do not reset an account that also contains unrelated calendars.

Discover collections and inspect the proposed mapping:

```sh
vdirsyncer discover clearhead
```

The local charter directories and remote CalDAV collections should correspond. Resolve naming surprises before the first synchronization. Discovery records the current collection set; when ClearHead later creates a VTODO in a new charter directory, run discovery again and explicitly approve the matching remote collection. Collection creation is intentionally not automated by the timer.

## Seed a fresh remote

For a genuinely new, dedicated remote collection set, the local VTODO vdir can be the initial source:

```sh
vdirsyncer sync clearhead
```

Do not assume that "remote wins" or "local wins" is safe when both sides already contain data. Back up both sides, choose the intended source explicitly, and reset only the dedicated ClearHead collections before seeding.

After seeding, verify that every recurring Plan is still readable:

```sh
clearhead read plans
```

## Prove the loop manually

Do not automate the composition until both directions work manually.

### Calendar to ClearHead

1. Create a standalone task in the VTODO-capable client.
2. Give it a distinctive title, priority, and category.
3. Synchronize the transport:

   ```sh
   vdirsyncer sync clearhead
   ```

4. Preview ClearHead reconciliation:

   ```sh
   clearhead sync calendar --dry-run
   ```

5. Apply it:

   ```sh
   clearhead sync calendar
   ```

6. Confirm that a new root Action appears in the charter selected by the resource's collection directory.

### ClearHead to calendar

1. Add or edit an Action, including a priority or context.
2. Project it into the vdir:

   ```sh
   clearhead sync calendar
   ```

3. Push the vdir change:

   ```sh
   vdirsyncer sync clearhead
   ```

4. Confirm that the client shows the updated VTODO.

### Acceptance checklist

Before enabling a timer, verify:

- calendar-created VTODO → new Action;
- Action title and description edits → calendar client;
- `PRIORITY` values 1–9 in both directions;
- contexts ↔ `CATEGORIES`;
- `STATUS:CANCELLED` → cancelled Action;
- deleting a projected resource causes ClearHead to recreate it rather than cancelling or deleting the Action;
- an arbitrary client-generated UID is retained after synchronization;
- alarms and unrecognized calendar properties survive ClearHead-owned field updates.

## Routine synchronization order

A complete cycle is:

```sh
vdirsyncer sync clearhead
clearhead sync calendar
vdirsyncer sync clearhead
```

The first transport pass pulls client edits. ClearHead then reconciles the local vdir with Actions. The second transport pass pushes ClearHead changes.

Do not run multiple copies of this cycle concurrently. ClearHead uses a workspace writer lock, while vdirsyncer maintains its own synchronization state; neither tool can make the combined three-command transaction atomic.

## systemd user timer

After the manual acceptance test passes, a user service can preserve the required ordering.

`~/.config/systemd/user/clearhead-calendar-sync.service`:

```ini
[Unit]
Description=Synchronize ClearHead VTODOs through CalDAV
After=network-online.target
Wants=network-online.target

[Service]
Type=oneshot
WorkingDirectory=%h
ExecStart=/usr/bin/vdirsyncer sync clearhead
ExecStart=/usr/local/bin/clearhead sync calendar
ExecStart=/usr/bin/vdirsyncer sync clearhead
```

Adjust executable paths using `command -v clearhead` and `command -v vdirsyncer`.

`~/.config/systemd/user/clearhead-calendar-sync.timer`:

```ini
[Unit]
Description=Periodically synchronize ClearHead calendar tasks

[Timer]
OnBootSec=2m
OnUnitActiveSec=5m
Persistent=true

[Install]
WantedBy=timers.target
```

Enable it:

```sh
systemctl --user daemon-reload
systemctl --user enable --now clearhead-calendar-sync.timer
systemctl --user list-timers clearhead-calendar-sync.timer
```

Inspect failures rather than blindly retrying them:

```sh
systemctl --user status clearhead-calendar-sync.service
journalctl --user -u clearhead-calendar-sync.service
```

Multiple `ExecStart` lines are intentional for a `Type=oneshot` service. If a step fails, systemd stops the sequence; this prevents a later transport pass from hiding an unresolved ClearHead or vdirsyncer conflict.

## Conflicts and failure boundaries

Two independent conflict layers exist:

1. **vdirsyncer conflicts** concern two versions of an iCalendar resource.
2. **ClearHead conflicts** concern independently edited Action and VTODO fields relative to their last agreed merge base.

Resolve the layer reporting the conflict. Do not delete synchronization state or choose a blanket winner merely to make the next timer run green.

Useful diagnostics:

```sh
vdirsyncer sync clearhead
clearhead sync calendar --dry-run
clearhead debug
clearhead doctor
```

A missing VTODO resource does not mean cancellation. Use an explicit VTODO `STATUS:CANCELLED` edit or `clearhead cancel action` for lifecycle changes.

## Reset and reseed

When the dedicated test deployment is disposable:

1. stop the timer;
2. back up the local plans vdir and remote collections;
3. decide whether local ClearHead data or the remote collection is canonical;
4. clear only the dedicated destination collections and the corresponding vdirsyncer status for this pair, following vdirsyncer's documentation;
5. run discovery again;
6. perform one transport sync;
7. run `clearhead sync calendar --dry-run` and inspect every proposed change;
8. apply the reconciliation and complete the acceptance checklist;
9. re-enable the timer.

Never reset Radicale's entire storage directory as part of this procedure. Radicale's private files are not the ClearHead integration surface.

## Replaceable components

This recipe chooses Radicale, vdirsyncer, and systemd, but none are required by the ClearHead data contract. Another CalDAV server, filesystem synchronization tool, scheduler, or manual process can replace them as long as ClearHead sees a standards-compliant VTODO vdir at its configured `plan_path`.
