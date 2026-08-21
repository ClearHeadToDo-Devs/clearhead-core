# ClearHead CLI Cookbook

This cookbook contains non-normative recipes for composing `clearhead` with other command-line and self-hosted tools. The recipes describe working operations, deployment boundaries, and failure recovery; they do not redefine the ClearHead data contracts.

Normative behavior lives in the [ClearHead specifications](https://github.com/ClearHeadToDo-Devs/specifications), especially the [iCalendar VTODO projection specification](https://github.com/ClearHeadToDo-Devs/specifications/blob/master/ics_schedule_spec.md).

## Calendar compositions

- [Radicale and vdirsyncer](./radicale-vdirsyncer.md) — expose ClearHead's filesystem vdir through CalDAV without coupling ClearHead to a server.

## Recipe conventions

Each recipe should state:

1. the components and ownership boundaries;
2. the files or protocols connecting them;
3. a minimal configuration;
4. a manual acceptance test before automation;
5. routine operation and ordering;
6. conflict, backup, and reset procedures;
7. which details are replaceable implementation choices.

Recipes should prefer secret-manager commands over plaintext credentials and must not recommend editing another application's private storage.
