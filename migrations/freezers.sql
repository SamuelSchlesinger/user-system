create table freezers (
  id text primary key unique not null,
  code text unique not null,
  name text unique not null,
  rack_width int8 not null,
  rack_height int8 not null,
  racks_per_shelf int8 not null,
  shelf_number int8 not null,
  creation_date timestamp with time zone not null
);
