create table mouse_properties (
  property_type text not null references mouse_property_types(name),
  name text unique not null,
  description text not null,
  creation_date timestamp with time zone not null,
  primary key (name, property_type)
);
