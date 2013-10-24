#include <iostream>
#include <memory>
#include <unordered_map>
#include <vector>

typedef std::vector<uint32_t> root_info_t;
typedef std::unordered_map<void*, root_info_t*>
  stack_map_inner_t;

class stack_map_t : public stack_map_inner_t
{
 public:
  stack_map_t();

  bool find_root_info(void* ip, root_info_t &root_info_ptr)
  {
    auto root_iter = find(ip);
    if (root_iter == end())
      {
        return false;
      }
    else
      {
        root_info_ptr = *root_iter->second;
        return true;
      }
  }

  std::vector<std::unique_ptr<root_info_t>> root_infos;
};
