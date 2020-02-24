create table samples (
  id text primary key unique not null,
  code text unique not null,
  mouse text not null references mice(id),
  box text not null references boxes(id),
  sample_type text not null references sample_types(name),
  notes text not null,
  creation_date timestamp with time zone not null
);
