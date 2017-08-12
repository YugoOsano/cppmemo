// http://qiita.com/imadedede/items/180cf808eb74f5dda730

/*
 * C++ で State パターンの練習
 * Wikipedia の State パターン (Java) を C++ 風に直してみた
 * ヘッダ版
 */
#include <string>
#include <memory>

// 前方宣言
class StateContext;

// 状態のための抽象クラス
class State{
public:
  virtual ~State() {
  }
  virtual void WriteString(StateContext      *state_context,
			   const std::string &str) = 0;
};

// 状態クラスその１
class StateBasic : public State{
public:
    void WriteString(StateContext *state_context, const std::string &str);
};

// 状態クラスその２
class StatePlus : public State{
public:
    StatePlus(void): cnt() {
    }
    void WriteString(StateContext *state_context, const std::string &str);
private:
    int cnt;
};

// 文脈クラス
class StateContext{
public:
  StateContext(void): mState(new StateBasic()), mNext() {
    //StateContext(void): mState(std::make_unique<StateBasic>()), mNext() {
  }
    // 次の状態を準備
    void SetNextState(State *state) {
        if (mNext) delete mNext;
        mNext = state;
    }
    // 状態ごとに異なる動作を行う
    void WriteString(const std::string &str) {
        mState->WriteString(this, str);
        StateToForward();
    }
private:
  State *mState;
  State *mNext;
  //  std::unique_ptr<State> mState;
  //std::unique_ptr<State> mNext;
  // 状態を進めてみる
    void StateToForward(void) {
        if (!mNext) return;
        if (mState) delete mState;
        mState = mNext;
        mNext = 0;
        return;
    }
};

/*
 * C++ で State パターンの練習
 * Wikipedia の State パターン (Java) を C++ 風に直してみた
 * 実装版
 */
#include <iostream>
//#include "StatePattern.h"

// 状態その１　文字列をそのまま出力
void StateBasic::WriteString(StateContext *state_context, const std::string &str) {
    std::cout << str << std::endl;
    state_context->SetNextState(new StatePlus());
}

// 状態その２　文字列にプラスを追加して出力
void StatePlus::WriteString(StateContext *state_context, const std::string &str) {
    std::cout << str << "+" << std::endl;
    if (++cnt >1)
        state_context->SetNextState(new StateBasic());
}

// エントリポイント
int main(void){
    StateContext state_context;
    state_context.WriteString("aaa");
    state_context.WriteString("bbb");
    state_context.WriteString("ccc");
    state_context.WriteString("ddd");
    state_context.WriteString("eee");
    state_context.WriteString("fff");
    return 0;
}
