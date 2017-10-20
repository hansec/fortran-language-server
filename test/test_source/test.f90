PROGRAM myprog
USE test_free, ONLY: scaled_vector
TYPE(scaled_vector) :: myvec
CALL myvec%set_scale(scale)
END PROGRAM myprog
