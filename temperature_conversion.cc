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
  //-- conversion
  template <Scale S, Scale R>
  struct ConversionTraits {
    static double Convert(const double value)=delete;
  };
  template <>
  struct ConversionTraits<Scale::Celsius, Scale::Fahrenheit> {
    static double Convert(const double value) {
      return (value * 9) / 5 + 32;
    }
  };
  template <>
  struct ConversionTraits<Scale::Fahrenheit, Scale::Celsius> {
    static double Convert(const double value) {
      return (value - 32) * 5 / 9;
    }
  };
  template <Scale R, Scale S>
  constexpr Quantity<R> temperature_cast(const Quantity<S> q) {
    return Quantity<R>(ConversionTraits<S,R>::Convert(
	static_cast<double>(q)));
  }
} // end of temperature
namespace temperature::scale_literal {
  constexpr Quantity<Scale::Celsius> operator "" _deg
          (const long double amount) {
    return Quantity<Scale::Celsius> {static_cast<double>(amount)};
  }
} // end of scale_literal

int main () {
  // for all enum, functions, structs
  using namespace temperature;
  //-- using for _deg
  using namespace temperature::scale_literal;
  auto t1{ 36.5_deg };
  auto tf = temperature_cast<Scale::Fahrenheit>(t1);
  return 0;
}
