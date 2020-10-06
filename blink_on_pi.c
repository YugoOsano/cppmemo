// control Raspberry Pi by C language
// https://qiita.com/Brutus/items/54605b634c96b0b1e5cf

#include <wiringPi.h>
#define pin 7 
int main (void)
{
  wiringPiSetup () ;
  pinMode (pin, OUTPUT) ;
  for (;;)
  {
    digitalWrite (pin, HIGH) ; delay (500) ;
    digitalWrite (pin,  LOW) ; delay (500) ;
  }
  return 0 ;
}
