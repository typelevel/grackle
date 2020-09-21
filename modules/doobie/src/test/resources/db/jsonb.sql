CREATE TABLE records (
    id Int PRIMARY KEY,
    record JSONB
);

COPY records (id, record) FROM STDIN WITH DELIMITER '|';
1|{"bool":true,"int":1,"float":1.3,"string":"foo","id":"Foo","array":[1,2,3],"choice":"ONE","object":{"id":"obj0","aField":27},"children":[{"id":"a0","aField":11},{"id":"b0","bField":"wibble"}]}
2|{"bool":false,"int":2,"float":2.4,"string":"bar","id":"Bar","array":[4,5,6],"choice":"TWO","object":{"id":"obj1","aField":28},"children":[{"id":"a1","aField":12},{"id":"b1","bField":"wobble"}]}
3|{"bool":true,"int":3,"float":3.5,"string":"baz","id":"Baz","array":[7,8,9],"choice":"THREE","object":{"id":"obj2","aField":29},"children":[{"id":"a2","aField":13},{"id":"b2","bField":"quux"}]}
\.
