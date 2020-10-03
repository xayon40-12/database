drop table if exists checkedout;
drop table if exists tools;
drop table if exists users;

create table users (
	id integer primary key,
	username text
);

create table tools (
	id integer primary key,
	name text,
	description text,
	lastReturned text,
	timesBorrowed integer
);

create table checkedout (
	user_id integer,
	tool_id integer
);

insert into users (username) values ('xayon');

insert into tools (name,description,lastreturned,timesborrowed)
values ('hammer','hits stuff','2017-01-01',0);

insert into tools (name,description,lastreturned,timesborrowed)
values ('saw','cuts stuff','2018-04-21',0);
