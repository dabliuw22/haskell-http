CREATE TABLE products (
    id VARCHAR PRIMARY KEY,
    name VARCHAR NOT NULL,
    stock FLOAT8 NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL
);

INSERT INTO products (id, name, stock, created_at)
    VALUES ('d91ae396-42e7-4483-a3ef-e729c486980f', 'p1', 10.0, now());