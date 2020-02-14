create table objects (
  id text primary key,
  name text unique not null,
  contents bytea not null,
  creation_date timestamp with time zone not null
);
