CREATE TABLE QUALIFIED.union_order_entities (
    id VARCHAR2(100) NOT NULL PRIMARY KEY,
    entity_type VARCHAR2(100) NOT NULL,
    name VARCHAR2(100) NOT NULL
);

INSERT INTO QUALIFIED.union_order_entities (id, entity_type, name) VALUES ('1', 'ItemA', 'Charlie');
INSERT INTO QUALIFIED.union_order_entities (id, entity_type, name) VALUES ('2', 'ItemB', 'Alpha');
INSERT INTO QUALIFIED.union_order_entities (id, entity_type, name) VALUES ('3', 'ItemA', 'Bravo');
INSERT INTO QUALIFIED.union_order_entities (id, entity_type, name) VALUES ('4', 'ItemB', 'Delta');
