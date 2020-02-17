create table sessions (
  id text primary key,
  owner text not null references users(id) on delete cascade,
  creation_date timestamp with time zone not null,
  token text unique not null,
  expiration_date timestamp with time zone not null
);
