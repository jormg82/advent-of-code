#include <stdio.h>

#define MAX(a,b) (((a)>(b))?(a):(b))
#define MIN(a,b) (((a)<(b))?(a):(b))


typedef enum {Missile=1, Drain, Shield, Poison, Recharge} Spell;
typedef enum {PlayerWins, Continue, Abort} Result;


void trySpell(Spell spell);

Result applyEffects();

Result tryMissile();
Result tryDrain();
Result tryShield();
Result tryPoison();
Result tryRecharge();

void trySpells();

Result turnBoss();


typedef struct State {
  int bossHit;
  int bossDamage;
  int playerHit;
  int playerMana;
  int playerShield;
  int spentMana;
  int shieldTimer;
  int poisonTimer;
  int rechargeTimer;
} State;

int minMana = 999999999;

//State stack[1000] = {{4, 8, 10, 250, 0, 0, 0, 0, 0}};
State stack[1000] = {{58, 9, 50, 500, 0, 0, 0, 0, 0}};
int top = 0;


Result applyEffects()
{
  if (stack[top].poisonTimer > 0) {
    stack[top].bossHit -= 3;
    stack[top].poisonTimer--;
  }

  if (stack[top].rechargeTimer > 0) {
    stack[top].playerMana += 101;
    stack[top].rechargeTimer--;
  }

  if (stack[top].shieldTimer > 0) {
    stack[top].playerShield = 7;
    stack[top].shieldTimer--;
  }

  if (stack[top].bossHit <= 0)
    return PlayerWins;
  else
    return Continue;
}


Result tryMissile()
{
  stack[top].playerMana -= 53;
  stack[top].spentMana += 53;

  if (stack[top].playerMana >= 0) {
    stack[top].bossHit -= 4;
    
    if (stack[top].bossHit > 0)
      return Continue;
    else
      return PlayerWins;
  }
  else
    return Abort;
}


Result tryDrain()
{
  stack[top].playerMana -= 73;
  stack[top].spentMana += 73;

  if (stack[top].playerMana >= 0) {
    stack[top].bossHit -= 2;
    stack[top].playerHit += 2;
    
    if (stack[top].bossHit > 0)
      return Continue;
    else
      return PlayerWins;
  }
  else
    return Abort;
}


Result tryShield()
{
  stack[top].playerMana -= 113;
  stack[top].spentMana += 113;

  if (stack[top].playerMana >= 0 &&
      stack[top].shieldTimer <= 0) {

    stack[top].shieldTimer = 6;
    stack[top].playerShield = 7;
    
    return Continue;
  }
  else
    return Abort;
}


Result tryPoison()
{
  stack[top].playerMana -= 173;
  stack[top].spentMana += 173;

  if (stack[top].playerMana >= 0 &&
      stack[top].poisonTimer <= 0) {

    stack[top].poisonTimer = 6;
    
    return Continue;
  }
  else
    return Abort;
}


Result tryRecharge()
{
  stack[top].playerMana -= 229;
  stack[top].spentMana += 229;

  if (stack[top].playerMana >= 0 &&
      stack[top].rechargeTimer <= 0) {

    stack[top].rechargeTimer = 5;
    
    return Continue;
  }
  else
    return Abort;
}


Result turnBoss()
{
  stack[top].playerHit -= MAX(8-stack[top].playerShield, 1);

  if (stack[top].shieldTimer <= 0)
    stack[top].playerShield = 0;

  if (stack[top].playerHit > 0)
    return Continue;
  else
    return Abort;
}


void trySpell(Spell spell)
{
  ++top;
  stack[top] = stack[top-1];

  Result res = applyEffects();
  if (res == Continue) {
    switch (spell) {
    case Missile:  res = tryMissile(); break;
    case Drain:    res = tryDrain(); break;
    case Shield:   res = tryShield(); break;
    case Poison:   res = tryPoison(); break;
    case Recharge: res = tryRecharge(); break;
    default: fprintf(stderr, "Caso no reconocido\n");
    }

    if (res == Continue) {
      if ((res = applyEffects()) == Continue) 
        res = turnBoss();
    }
  }

  if (res == PlayerWins) {
    // annotate minMana
    if (minMana > stack[top].spentMana)
      fprintf(stderr, "Encontrado min=%d\n", stack[top].spentMana);
    minMana = MIN(stack[top].spentMana, minMana);
  }
  else if (res == Continue)
    trySpells();

  --top;
}


void trySpells()
{
  for (Spell spell = Recharge; spell >= Missile; --spell)
    trySpell(spell);
}


int main()
{
  trySpells();

  fprintf(stderr, "minMana = %d\n", minMana);

  return 0;
}
