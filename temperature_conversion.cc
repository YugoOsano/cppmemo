namespace temperature {
  enum class Scale {
		    Celsius,
		    Fahrenheit,
		    Kelvin
  };
  template <Scale S>
  class Quantity {
  public:
    constexpr explicit Quantity(const double a) :
      amount_(a) {}
    explicit operator double () const {return amount_;}
  private:
    const double amount_;
  };  
} // end of temperature

int main () {
  return 0;
}
