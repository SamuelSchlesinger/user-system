# Freezer Management System

A freezer sample inventory management tracking service for the purpose of
automating organizational tasks related to the inventory and samples.

## The Problem

There is a lot of manual work associated with this management at the moment,
and the current system, consisting of one excel spreadsheet with different
tabs for each shelf as well as various other spreadsheets which store other
sorts of information about the samples. These other spreadsheets are difficult
to maintain and interrelate with each other and the inventory spreadsheet.
These are collaboratively maintained on DropBox and their contents are not
necessarily well documented or understood by everyone in the lab.

## The Solution

The tubes are very small so you can only put so much information directly
on them, and after long periods of storage you can lose important information
associated with that sample. Thus, a solution is required which does not simply
encode the information on the sample, but maintains a database of the knowledge
in a well documented, searchable, and easily updatable form.

Thus, our task is to understand the various spreadsheets which are currently
in use, to create a database schema which reflects all of the information
that is currently being maintained, and an interface for the lab employees to
update and access this data.

## Installing/Running

To run this, you need to have PostgreSQL running locally on port 5432 with a database called "freezers".
To startup, run the commands:
```bash
export FREEZE_LOCATION=<this-folder>
stack run migrate
stack run freezer-management
```
If you want to drop the various tables that this adds to that postgres database, you can run this script
and get rid of them all. 
```bash
stack run drop
```

## Module Structure

![Module Structure](mods.png)
