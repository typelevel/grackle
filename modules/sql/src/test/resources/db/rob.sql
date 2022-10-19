

create table t_program (
  c_program_id  varchar not null primary key
);

create table t_observation (
  c_program_id         varchar    not null    references t_program(c_program_id),
  c_observation_id     varchar    not null primary key 
)