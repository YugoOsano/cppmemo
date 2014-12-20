// http://d.hatena.ne.jp/meison_amsl/20140811/1407721701
// https://github.com/AtsushiSakai/cpp/blob/master/DesignPattern/Strategy/Strategy.cpp

// g++ -std=c++0x -Wall strategy_pattern.cc
/**
 * @file: Strategy.cpp
 *
 * @brief: Strategy model of the Design pattern.
 *
 * @author: Atsushi Sakai
 *
 * @copyright (c): 2014 Atsushi Sakai
 *
 * @license : GPL Software License Agreement
 **/
#include <iostream>
#include <vector>
using namespace std;
/**
   @brief 平均計算エンジン用抽象クラス
*/
class AbstractMeanCalc{
public:
  virtual void MeanCalc(const vector<float> &data)=0;
};
/**
   @brief 相加平均計算用クラス
*/
class ArithmeticMean : public AbstractMeanCalc{
public:
  void MeanCalc(const vector<float> &data){
    int ndata=data.size();
    float mean=0.0;
    for(int i=0;i<ndata;i++){
      mean+=data[i];
    }
    mean/=ndata;
    cout<<"ArithmeticMean:"<<mean<<endl;
  }
private:
};
/**
 * @brief 調和平均計算用クラス
*/
class HarmonicMean : public AbstractMeanCalc{
public:
  void MeanCalc(const vector<float> &data){
    int ndata=data.size();
    float mean=0.0;
    for(int i=0;i<ndata;i++){
      mean+=1.0/data[i];
    }
    mean*=ndata;
    cout<<"HarmonicMean:"<<mean<<endl;
  }
private:
};
/**
 * @brief 計算機クラス 平均計算メソッドはStrategyパターンで変更可能
 */
class Calculator{
public:
  Calculator(AbstractMeanCalc *meanCalc){
    meanCalc_=meanCalc;
  }
  /**
   * @brief 平均を計算する関数
   */
  void MeanCalc(const vector<float> &data){
    meanCalc_->MeanCalc(data);
  }
private:
  AbstractMeanCalc *meanCalc_;//平均計算用エンジン
};
int main(void){
  cout<<"Strategy Method Pattern Sample Start!!"<<endl;
  //計算用元データ
  vector<float> data{5.2,10.1,40.8};//C++11モードでコンパイルしないとエラーになる
  //相加平均を計算する計算機
  Calculator calc(new ArithmeticMean());
  calc.MeanCalc(data);
  //調和平均を計算する計算機
  Calculator calc2(new HarmonicMean());
  calc2.MeanCalc(data);
  return 0;
}
