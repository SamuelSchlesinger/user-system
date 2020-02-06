create table executed_migrations (
  file_name text primary key unique not null,
  creation_date timestamp with time zone not null
);
