create table mice (
  id text primary key unique not null,
  tag text not null,
  date_of_birth timestamp with time zone not null,
  box text not null references boxes(id),
  date_of_tissue_collection timestamp with time zone not null,
  creation_date timestamp with time zone not null
);
