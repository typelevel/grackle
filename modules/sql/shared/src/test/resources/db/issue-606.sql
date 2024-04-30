BEGIN;

SET client_encoding = 'UTF8';

CREATE TABLE richtext (
	id numeric(20) NOT NULL,
	html text NULL,
	plain text NULL
);

CREATE TABLE instruction (
	id numeric(20) NOT NULL,
	description numeric(20) NULL
);

CREATE TABLE instructioncall (
	id numeric(20) NOT NULL,
	caller numeric(20) NOT NULL,
	callee numeric(20) NULL,
	description numeric(20) NULL,
	"comments" numeric(20) NULL
);

DROP VIEW IF EXISTS callsequence;

CREATE OR REPLACE VIEW callsequence
AS SELECT
    c.id AS stepkey,
    c.caller,
    c.callee,
    c.callee IS NULL AS textual,
    i.description -- i.description is the underlying mistake which provokes #606. Avoid it by using c.description.
   FROM instructioncall c
     LEFT JOIN instruction i ON c.callee = i.id;

INSERT INTO richtext (id,html,plain) VALUES
	 (63,'<html>Lorem Ipsum</html>','foo'),
	 (64,'<html>Lorem Ipsum</html>','meep'),
	 (65,'<html>Lorem Ipsum</html>',NULL),
	 (61,'<html>Lorem Ipsum</html>',NULL),
	 (62,'<html>Lorem Ipsum</html>',NULL),
	 (66,'<html>Lorem Ipsum</html>',NULL),
	 (60,'<html>Lorem Ipsum</html>',NULL);

INSERT INTO instruction (id,description) VALUES
	 (11,63),
	 (12,64),
	 (10,65);

INSERT INTO instructioncall (id,caller,callee,description,"comments") VALUES
	 (50,10,NULL,66,60),
	 (51,10,12,NULL,61),
	 (52,10,11,NULL,62);

COMMIT;
