CREATE TABLE products (
    id VARCHAR PRIMARY KEY,
    name VARCHAR NOT NULL,
    stock FLOAT8 NOT NULL
);

INSERT INTO products (id, name, stock)
    VALUES ('d91ae396-42e7-4483-a3ef-e729c486980f', 'p1', 10.0);