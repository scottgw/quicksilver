%.o: %.c
	$(CC) $< -c $(CFLAGS)

%.o: %.cpp
	$(CXX) $< -c $(CXXFLAGS)
