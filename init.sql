create table links(
    url varchar(255) primary key,
    original_url varchar(255),
    adjective varchar(255),
    noun varchar(255), 
    number integer,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
)