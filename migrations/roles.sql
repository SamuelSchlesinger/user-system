create type role as enum ('Read', 'Edit', 'Collaborator', 'Owner');

create table session_roles (
  session text not null references sessions(id),
  role role not null,
  object text not null references objects(id),
  creation_date timestamp with time zone not null,
  primary key (session, object)
);

create table user_roles (
  user_ text not null references users(id),
  role role not null,
  object text not null references objects(id),
  creation_date timestamp with time zone not null,
  primary key (user_, object)
);
