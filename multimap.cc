// Reference:
// http://qiita.com/satoruhiga/items/957d69baceaaf68a8eb3

#include <map>
#include <vector>
#include <iostream>

struct Value{
  int a;
  int b;
};

class Property{
public:
  typedef std::multimap<int, Value>           MultiMapValue;
  typedef std::multimap<int, Value>::iterator MultiMapValueIter;

  void setValue(int id, Value value){
    Map[id] = value;
  }
  Value getValue(int id){
    return Map[id];
  }
  //-- for the multimap
  void setValueMulti(int id, Value value){
    MultiMap.insert(std::make_pair(id, value));
  }
  void getValueMulti(int id, std::vector<Value>& vec){
    vec.clear();
    
    std::pair<MultiMapValueIter, MultiMapValueIter> range 
      = MultiMap.equal_range(id);

    for(MultiMapValueIter it = range.first; it != range.second; it++)
      vec.push_back(it->second);
  }
  //-- simple getter --
  const MultiMapValue& GetMultiMap() const {return MultiMap;}
protected:
  std::map<int, Value> Map;
  MultiMapValue        MultiMap;
};

int main()
{
  Property property;

  Value v;

  for(int i=0; i<10; i++){
    v.a = 1000*i + 1;
    v.b = 1000*i + 2;

    property.setValue(i, v);
    //-- coming of the same key will replace the existing key-value set
    if (i==5) property.setValue(i, Value{-1,-1});

    property.setValueMulti(0, v);
  }

  for(int i=0; i<10; i++){
    std::cout << "v[" << i << "]: " 
	      << property.getValue(i).a << ","   
	      << property.getValue(i).b << std::endl;
  }
  std::vector<Value>           vecValue;
  property.getValueMulti(0, vecValue);
  for(std::vector<Value>::iterator it = vecValue.begin();
      it != vecValue.end(); it++){
    std::cout << "MultiMap v[0]: " 
	      << (*it).a << "," 
	      << (*it).b << std::endl;
  }

  const std::pair<std::multimap<int, Value>::const_iterator,
		  std::multimap<int, Value>::const_iterator>&
    value_range = property.GetMultiMap().equal_range(0);
  std::cout << "\nloop over iterator taken by equal_range(0):" << std::endl;
  for (auto iter = value_range.first;
       iter     != value_range.second; iter++) {
    std::cout << iter->first << ": "
	      << iter->second.a << ", "
	      << iter->second.b << std::endl;
  }
  return 0;
}
