# Check staged ClearHead files before committing

A pre-commit hook can stop an invalid workspace from entering Git history. This
hook is optional and independent of the merge driver: it runs `clearhead
doctor` on the **staged** workspace, not on whatever happens to be in the
working tree. Warnings are allowed; violations block the commit.

Continue in the scratch project from [Merge ClearHead actions with Git](./merge-actions-with-git.md).
You'll need `jq` as well as the same ClearHead build. The hook uses `jq` to
confirm that a status of 1 came with a doctor report, rather than allowing a
CLI failure to pass as a warning.

## Install the hook

Run from the repository root. If your project already has hooks, add this
check to the existing pre-commit flow instead of replacing its hook or changing
`core.hooksPath`.

```sh
mkdir -p .githooks
cat > .githooks/pre-commit <<'HOOK'
#!/bin/sh

git diff --cached --quiet -- .clearhead/
case $? in
    0) exit 0 ;; # Nothing in ClearHead is staged.
    1) ;;        # There are staged changes to check.
    *) exit 1 ;;
esac

snapshot=$(mktemp -d) || exit 1
trap 'rm -rf "$snapshot"' 0
git ls-files --cached -z -- .clearhead/ > "$snapshot/paths" || exit 1
git checkout-index --prefix="$snapshot/" --stdin -z < "$snapshot/paths" || exit 1
[ -d "$snapshot/.clearhead" ] || exit 0 # The entire workspace was removed.

# Use project scope regardless of this contributor's personal settings.
printf '%s\n' '{"default_to_user_scope":false}' > "$snapshot/config.json" || exit 1
report=$(cd "$snapshot" && clearhead --config "$snapshot/config.json" doctor --json)
status=$?
if [ "$status" -le 1 ] && [ -n "$report" ] &&
    printf '%s\n' "$report" | jq -e '.findings | type == "array"' >/dev/null
then
    exit 0
fi

printf '%s\n' "$report" >&2
printf '%s\n' 'ClearHead: fix the staged workspace and try again.' >&2
exit 1
HOOK
chmod +x .githooks/pre-commit
git config --local core.hooksPath .githooks
git add .githooks/pre-commit
git commit -m 'Check staged ClearHead files before committing'
```

The hook is tracked, but activating it with `git config` is per clone. Git
hooks are a local guard, not a substitute for a CI check if your project needs
server-side enforcement.

## Try a rejected commit, then fix it

In the scratch project only, stage an unresolved conflict, then restore the
valid working copy **without staging it**. This shows why checking the staged
snapshot matters:

```sh
clearhead update action release-notes --name 'Publish release notes'
actions=.clearhead/charters/next.actions
valid=$(mktemp)
cp "$actions" "$valid"
printf '\n<<<<<<< ours\n=======\n>>>>>>> theirs\n' >> "$actions"
git add -- "$actions"
cp "$valid" "$actions"
rm "$valid"

if git commit -m 'Try committing an unresolved conflict'; then
    printf '%s\n' 'Unexpected success: the hook did not reject the staged conflict.' >&2
    exit 1
fi
```

The hook reports **E010** and refuses the commit, even though the working copy
looks fine. Stage the fix and run the commit again:

```sh
git add -- "$actions"
git commit -m 'Publish release notes'
```
