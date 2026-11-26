# Merge ClearHead actions with Git

In this tutorial, two branches edit the name and priority of the same action.
Git normally merges by line; the ClearHead driver merges by action UUID and
keeps both edits. You need Git with an author identity set and a ClearHead
build that includes `merge-actions`.

## Set up a project

Start in a new scratch repository. If you already have a Git-backed ClearHead
workspace, check `merge-actions --help` and then skip to the attributes setup
from your repository root. Add the rules to any existing attributes file
rather than replacing it.

```sh
clearhead merge-actions --help
mkdir clearhead-git-demo
cd clearhead-git-demo
git init -b main
clearhead init --name release-demo
```

Share the file rules, but register the executable locally in each clone. The
rules live under `.clearhead/` to leave other files in the project alone. The
last rule keeps completed-action archives on Git's union merge, which retains
both sides' appends.

```sh
printf '%s\n' '*.actions merge=clearhead-actions' '*.completed.actions merge=union' \
    > .clearhead/.gitattributes
git config --local merge.clearhead-actions.driver 'clearhead merge-actions %O %A %B'
git check-attr merge -- .clearhead/charters/next.actions .clearhead/charters/.completed.actions
```

The two paths should report `clearhead-actions` and `union`, respectively.
Commit `.clearhead/.gitattributes` so every clone gets the same rules. Each
contributor still needs the CLI on `PATH` and the `git config` command; without
registration, Git normally falls back to its text merge. For a private trial
without a tracked attributes change, put the rules in Git's local
`info/attributes` file instead (`git rev-parse --git-path info/attributes`), and
prefix the patterns with `.clearhead/**/` to keep the same scope.

## Merge two edits to one action

```sh
clearhead add action 'Draft release notes' --alias release-notes --priority 1
git add .clearhead/
git commit -m 'Track release notes'

git switch -c rename-notes
clearhead update action release-notes --name 'Outline release notes'
git add .clearhead/
git commit -m 'Rename the release-notes action'

git switch main
clearhead update action release-notes --priority 2
git add .clearhead/
git commit -m 'Set the release-notes priority'

git merge --no-edit rename-notes
clearhead show action release-notes
```

The merge should succeed. The action is now named **Outline release notes**
and has priority **2**, with its original UUID.

## When a merge stops

When both sides set the same field differently, or make incompatible changes to
the tree, Git stops. Open the conflicted `.actions` file, resolve the marked
region, and remove the marker lines. Keep the clean edits outside the markers.
Run `clearhead doctor`, then stage the resolved files and run
`git merge --continue`. Before resolution, `doctor` reports **E010** for
unresolved markers.

If an otherwise valid input has an action without a UUID, the driver uses
`git merge-file` for that file; it doesn't invent IDs or merge separate fields.
It refuses malformed input and won't write a fallback result with duplicate
UUIDs or conflict markers ClearHead would read as description text. In that
case Git still has the original versions for manual resolution.

This is an optional **Git** integration, not a sync method. Syncthing does not
run the driver or supply a common ancestor. If you have all three versions,
`clearhead merge-actions BASE OURS THEIRS` also works on copies outside Git;
it writes to `OURS`. Without the actual base, resolve the copies manually.
The driver covers `.actions` files, not JSON sidecars.

Next: [Check staged ClearHead files before committing](./check-clearhead-before-committing.md).
