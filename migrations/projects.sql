create table projects (
  id text primary key unique not null,
  name text unique not null,
  description text not null,
  creation_date timestamp with time zone not null
);
