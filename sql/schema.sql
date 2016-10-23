drop schema public cascade;
create schema public;
GRANT ALL ON SCHEMA public TO public;
COMMENT ON SCHEMA public IS 'standard public schema';
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

 -- TODO encrypt!
create table users (
  id uuid PRIMARY KEY /* newtype UserId */,
  email text UNIQUE NOT NULL,
  password text NOT NULL
);

create table sessions (
  id uuid PRIMARY KEY /* newtype SessionId */,
  userId uuid NOT NULL /* newtype UserId */,
  createdAt timestamp without time zone NOT NULL
);

-----------------------------------------------------

CREATE FUNCTION makeUser (IN email users.email%TYPE, IN password users.password%TYPE, OUT userId users.id%TYPE)
AS $$
  INSERT INTO users (id, email, password)
  VALUES (uuid_generate_v4(), email, password)
  RETURNING id;
$$ LANGUAGE SQL;

CREATE FUNCTION findUser (IN email users.email%TYPE, OUT userId users.id%TYPE)
AS $$
  SELECT id FROM users u WHERE u.email = email;
$$ LANGUAGE SQL;

CREATE FUNCTION validUser (IN email users.email%TYPE, IN password users.password%TYPE, OUT userId users.id%TYPE)
AS $$
  SELECT id FROM users u WHERE u.email = email AND u.password = password;
$$ LANGUAGE SQL;

CREATE FUNCTION createSession (IN userId users.id%TYPE, IN now sessions.createdAt%TYPE, OUT sessionId sessions.id%TYPE)
AS $$
  INSERT INTO sessions (id, userId, createdAt)
  VALUES (uuid_generate_v4(), userId, now)
  RETURNING id;
$$ LANGUAGE SQL;

CREATE FUNCTION getSession (IN sessionId sessions.id%TYPE)
RETURNS sessions
AS $$
  SELECT * FROM sessions
  WHERE id = sessionId;
$$ LANGUAGE SQL;
