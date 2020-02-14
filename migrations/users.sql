create table users (
  id text primary key,
  username text unique not null,
  passhash bytea not null,
  creation_date timestamp with time zone not null
);

create unique index username_is_unique on users(username);
