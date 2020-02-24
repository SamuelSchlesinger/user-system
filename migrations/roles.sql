create type role as enum ('Read', 'Edit', 'Collaborator', 'Owner');

create table session_roles (
  session text not null references sessions(id) on delete cascade,
  role role not null,
  box text not null references boxes(id) on delete cascade,
  creation_date timestamp with time zone not null,
  primary key (session, box)
);

create table user_roles (
  user_ text not null references users(id) on delete cascade,
  role role not null,
  box text not null references boxes(id) on delete cascade,
  creation_date timestamp with time zone not null,
  primary key (user_, box)
);
