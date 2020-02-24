create table boxes (
  id text primary key unique not null,
  code text not null references users(id) on delete cascade,
  project text not null references projects(id) on delete cascade,
  freezer text not null references freezers(id) on delete cascade,
  shelf int8 not null,
  rack int8 not null,
  slot_x int8 not null,
  slot_y int8 not null,
  creation_date timestamp with time zone not null
);
