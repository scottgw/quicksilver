%.o: %.c
	$(CC) $< -c $(CFLAGS)

