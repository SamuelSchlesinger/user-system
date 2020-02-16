# User System

A basic user system with objects and roles at the user and session level, with 
sessions stored in the database hashed with bcrypt. Useful for branching off of for 
other projects, kept tearing it out of old ones so I figured I'd give it a home.

## Technical Discussion

For the system, I use the servant web framework and postgresql-simple. This
allows me to very rapidly integrate raw SQL queries and Haskell in a way
that lets me experiment very quickly.

## Module Structure

![Module Structure](mods.png)
