# a genesis module.


def mandated_genesis_text():
    """Returns mandated genesis text.

       This text must be before any request_addition_text() calls.
    """
    return """
-- these two tables below are special.

-- special table - allowed requests.
CREATE TABLE allowed_requests
    ( request_id STRING PRIMARY KEY -- we expect hash here, for now any string would do.
    , request_text STRING
    );

-- special table - parameters for requests.
CREATE TABLE allowed_requests_params
    ( request_id STRING
    , request_param_name STRING
    , request_param_type STRING
    , CONSTRAINT UNIQUE (request_id, request_param_name)
    , CONSTRAINT FOREIGN KEY (request_id) REFERENCES allowed_requests(request_id)
    , CONSTRAINT CHECK (request_param_type = "S" OR request_param_type = "I" OR request_param_type = "P")
    );
"""

def _escape_char(c):
    if c == '"':
        return '\\"'
    if c == "'":
        return "\\'"
    return c

def _escape_sql(sql):
    return "".join(map(_escape_char, sql))

def _sql_string(str):
    return '"' + _escape_sql(str) + '"'

def request_text(id, sql, parameters):
    """Returns an SQL text that adds request and request's parameters into database"""
    result = list()
    result.append(
            "INSERT INTO allowed_requests (request_id, request_text) VALUES (" + _sql_string(id) + ", " _sql_string(sql) + ");"
        )
    result.append("")
    for (ptype, pnames) in parameters:
        if ptype == "integer":
            ty = "I"
        elif ptype == "string":
            ty = "S"
        elif ptype == "positive":
            ty = "P"
        else:
            throw Exception("unknown parameter type " + ptype)
        for name in pnames:
            if name[0] != ":":
                name = ":" + name
            result.append(
                "INSERT INTO allowed_requests_params " +
                "(request_id, request_param_name, request_param_type)" +
                "VALUES ("+_sql_string(id)+", "+_sql_string(name)+", "+_sql_string(ty)+");"
                )
    return "\n".join(result)
