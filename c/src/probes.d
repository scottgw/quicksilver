provider qs {
         probe ws_deque_steal_start();
         probe ws_deque_steal_fail();
         probe ws_deque_steal_end();

         probe stask_find_next_start();
         probe stask_find_next_end();
};