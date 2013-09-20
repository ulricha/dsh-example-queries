for i in 16 32 64 128 256 ; do dropdb organisation$i; createdb organisation$i && psql organisation$i < setup.sql ; done
