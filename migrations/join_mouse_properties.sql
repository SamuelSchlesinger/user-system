create table join_mouse_properties (
  mouse text not null references mice(id),
  mouse_property text not null references mouse_properties(name),
  mouse_property_type text not null references mouse_property_types(name),
  constraint mice_have_one_property_per_type unique(mouse, mouse_property_type),
  foreign key (mouse_property, mouse_property_type) references mouse_properties(name, property_type),
  primary key (mouse, mouse_property)
);
