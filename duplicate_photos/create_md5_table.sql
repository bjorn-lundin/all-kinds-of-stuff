begin;

create table md5s ( 
  id       int  not null , -- Primary Key
  md5      varchar(32)  not null , 
  fullname varchar not null , 
  filename varchar not null ,
  dirname  varchar not null,
  created  timestamp(3) without time zone 
  extension varchar,
  );

alter table md5s add constraint md5sP1 primary key (
  id
);

create index md5sI2 on md5s (
  md5
);

comment on table md5s            is 'Photo meta data';
comment on column md5s.id        is 'id';
comment on column md5s.md5       is 'md5sum';
comment on column md5s.fullname  is 'full filename';
comment on column md5s.filename  is 'filename';
comment on column md5s.dirname   is 'dirname';
comment on column md5s.extension is 'file extension';
comment on column md5s.created   is 'created timestamp';

commit;


--insert into md5s values (2,'17d680b37fc07232f3a44552de68090d','Masters/2012/19 okt 2012/IMG_1324.JPG','IMG_1324.JPG','Masters/2012/19 okt 2012','2022-12-13 12:23:34' , 'jpg'); 
--insert into md5s values (3,'17d680b37fc07232f3a44552de68090d','Masters/2012/19 okt 2012/IMG_1324.JPG','IMG_1324.JPG','Masters/2012/19 okt 2012',null , null); 

--alter table md55 add column extension varchar;

