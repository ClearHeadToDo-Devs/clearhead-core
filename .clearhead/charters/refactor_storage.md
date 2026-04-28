---
id: 019d236f-189b-7aef-94f0-daaeb7cfab8c
alias: refactor_storage
state: completed
---
# Refactor Storage Trait
Currently, we are relying on the workspace trait to do the work of turning domain objects into the actual storage backend we wanted.

this was from the time when we were planning to use CRDTs as the sync layer but since we moved to git as the primary sync layer, we can just make it a far simpler abstraction that will allow the various downstream products to use the same underlying logic all the way down to the file level and be relatively thin wrappers on top of a strong, core library

while this should be a relatively easy affair, care should be taken to make sure that the pain isnt too present and that what we create in return is worth the effort becuase there ARE quite a few places where we are utilizing the workspace trait so we need to be careful to do the full replacement

## Definition of Sucess

What we want is a clear underlying set of functions that both the CLI and the LSP can use to do the work of turning domain objects into files and files into domain objects, and that this is done in a way that is clean and easy to understand, and that allows us to have a clear separation of concerns between the storage layer and the domain logic layer
