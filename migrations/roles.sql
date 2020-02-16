create type role as enum ('Read', 'Edit', 'Collaborator', 'Owner');

create table session_roles (
  session text not null references sessions(id) on delete cascade,
  role role not null,
  object text not null references objects(id),
  creation_date timestamp with time zone not null,
  primary key (session, object)
);

create table user_roles (
  user_ text not null references users(id) on delete cascade,
  role role not null,
  object text not null references objects(id) on delete cascade,
  creation_date timestamp with time zone not null,
  primary key (user_, object)
);
