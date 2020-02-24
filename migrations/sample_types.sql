create table sample_types (
  name text primary key unique not null,
  description text not null,
  creation_date timestamp with time zone not null
);
