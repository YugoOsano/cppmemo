// tag dispatch-like member variable handling
#include <memory>

struct ProcessBase {
  virtual ~ProcessBase(){}
};
struct Sputtering : public ProcessBase {
  Sputtering(){}
};
struct Desorption : public ProcessBase {
};

namespace ns_LayerTag {
  struct Bare {
  };
  struct Polymer {
  };
}// end of LayerTag

struct ProcessList {
  ProcessList(ProcessBase* process_base) 
    : process_base_(process_base) {}
  virtual ~ProcessList() {
    if (process_base_) delete process_base_;
  }
  ProcessBase* process_base_;
};

class SetOfComponent {

public:
  SetOfComponent() : 
    process_list_sputtering_(ProcessList(new Sputtering)) {}
 
  template <class ProcessTag>
  const ProcessList& GetProcessList(const ProcessTag& process_tag) const {
    return GetProcessList (process_tag);
  }
protected:
  const ProcessList& 
  GetProcessList(const Sputtering& /*sputtering*/) const {
    return process_list_sputtering_;
  }

  ProcessList process_list_sputtering_;

};

int main () {
  std::unique_ptr<SetOfComponent> set_of_component
    (new SetOfComponent);

  const ProcessList& process_list
    = set_of_component->GetProcessList<Sputtering>(Sputtering());
						  

  return 0;
}
