#!/usr/bin/env python
# coding: utf-8

# In[263]:


import nest_asyncio
nest_asyncio.apply()
import asyncio
from poke_env.player.player import Player
from poke_env.player.random_player import RandomPlayer
from poke_env.environment.pokemon_type import PokemonType
import time
import json
#with open('type_chart.json','r') as f:
#    type_chart = json.load(f) 


# In[264]:


class Greedy(Player):
    def choose_move(self, battle):
        if battle.available_moves:
            opp = battle.opponent_active_pokemon
            best_move = max(battle.available_moves, key=lambda move: move.base_power*move.type.damage_multiplier(opp.type_1, opp.type_2))
            #print(best_move.type, opp.type_1, opp.type_2)
            #print(best_move.type.damage_multiplier(opp.type_1, opp.type_2))
            return self.create_order(best_move)
        else:
            return self.choose_random_move(battle)


# In[265]:


def DamCal(move , opp):
    dam = 0
    dam = dam + (move.base_power*move.type.damage_multiplier(opp.type_1, opp.type_2) * move.accuracy)
    
    
    if move.secondary != None:
        chance = 0
        secdam = 0
        
        for p in move.secondary:
            if 'status' in p:
                chance = p['chance']/100
                status = p['status']

                if(status == 'frz' or status == 'par'):
                    secdam = 20
                if(status == 'psn' or status == 'tox'):
                    secdam = 30
                if(status == 'brn'):
                    secdam = 50
                if(status == 'slp'):
                    secdam = 70    
                    
    dam = dam + move.accuracy * chance * secdam    
        
    return dam


# In[266]:


class Greedy_Switch(Player):        
    def choose_move(self, battle):
        switched_last_turn = False
        dyn = False
        if battle.available_moves:
            active = battle.active_pokemon
            opp = battle.opponent_active_pokemon
            
            #basic best_move
            #best_move = max(battle.available_moves, key=lambda move: move.base_power*move.type.damage_multiplier(opp.type_1, opp.type_2))
            best_move = max(battle.available_moves, key = lambda move: DamCal(move, opp))
            
            #dmg multiplier
            active_multiplier = active.type_1.damage_multiplier(opp.type_1, opp.type_2)
            if active.type_2 != None:
                active_multiplier = active_multiplier * active.type_2.damage_multiplier(opp.type_1, opp.type_2)            
            best_move_multiplier = best_move.type.damage_multiplier(opp.type_1, opp.type_2)
            opp_multiplier = opp.type_1.damage_multiplier(active.type_1, active.type_2) 
            if opp.type_2 != None:
                 opp_multiplier = opp_multiplier * opp.type_2.damage_multiplier(active.type_1, active.type_2)
            
            #Stats Calculator            
            active_hp = active.base_stats['hp'] * active.current_hp_fraction
            
            if opp.boosts['atk'] >= 0:            
                phy_dmg = opp.base_stats['atk'] * (2 + opp.boosts['atk']) / 2 - (active_hp+ active.base_stats['def'])
            if opp.boosts['atk'] < 0:
                phy_dmg = opp.base_stats['atk'] * 2 / (2 - opp.boosts['atk']) - (active_hp + active.base_stats['def'])
                
            if opp.boosts['spa'] >= 0:
                special_dmg = opp.base_stats['spa'] * (2 + opp.boosts['spa']) / 2 - (active_hp + active.base_stats['spd'])
            if opp.boosts['spa'] < 0:
                special_dmg = opp.base_stats['spa'] * 2 / (2 - opp.boosts['spa']) - (active_hp + active.base_stats['spd'])
            
            if opp.boosts['spe'] >= 0:
                spd_diff = opp.base_stats['spe'] * (2 + opp.boosts['spe']) / 2 - active.base_stats['spe']
            if opp.boosts['spe'] < 0:
                spd_diff = opp.base_stats['spe'] * 2 / (2 - opp.boosts['spe']) - active.base_stats['spe']

            #Switching_weak and slow
            if spd_diff > -5 and opp_multiplier > 1 and not battle.trapped and switched_last_turn == False:
                if  phy_dmg > -100 or special_dmg > -80 or opp_multiplier > 2:
                    for switch in battle.available_switches:
                        
                        switch_multiplier = switch.type_1.damage_multiplier(opp.type_1, opp.type_2)
                        if switch.type_2 != None:
                            switch_multiplier = switch_multiplier * switch.type_2.damage_multiplier(opp.type_1, opp.type_2)
                        
                        opp_switch_multiplier = opp.type_1.damage_multiplier(switch.type_1, switch.type_2)
                        if opp.type_2 != None:
                            opp_switch_multiplier = opp_switch_multiplier * opp.type_2.damage_multiplier(switch.type_1, switch.type_2)
                                
                        if opp_switch_multiplier <= 1 and switch_multiplier > opp_switch_multiplier:   
                            switched_last_turn = True                            
                            return self.create_order(switch)
            
            #Switching_unfair match
            if (best_move_multiplier < opp_multiplier) and not battle.trapped and switched_last_turn == False:
                for switch in battle.available_switches:
                    
                    switch_multiplier = switch.type_1.damage_multiplier(opp.type_1, opp.type_2)
                    if switch.type_2 != None:
                        switch_multiplier = switch_multiplier * switch.type_2.damage_multiplier(opp.type_1, opp.type_2)
                        
                    opp_switch_multiplier = opp.type_1.damage_multiplier(switch.type_1, switch.type_2)
                    if opp.type_2 != None:
                        opp_switch_multiplier = opp_switch_multiplier * opp.type_2.damage_multiplier(switch.type_1, switch.type_2)      
                    
                    if opp_switch_multiplier <= 1 and switch_multiplier > opp_switch_multiplier:
                        switched_last_turn = True 
                        return self.create_order(switch)
                    
                if (best_move_multiplier == 0) and not battle.trapped:
                    for switch in battle.available_switches:
                        return self.create_order(switch)
                
            switched_last_turn = False
            remain_team = len(battle.available_switches)
            
            #dynamax
            n = 0
            n = active.base_stats['hp'] + active.base_stats['atk'] + active.base_stats['def'] + active.base_stats['spa'] + active.base_stats['spd'] + active.base_stats['spe']
            
            #print(n)
            #print(remain_team)
            if n >= 600 and active_multiplier >= opp_multiplier and active.current_hp_fraction == 1 and battle.can_dynamax == True:
                dyn = True
            elif n >= 530 and active_multiplier >= opp_multiplier and active.current_hp_fraction == 1 and remain_team < 5 and battle.can_dynamax == True:
                dyn = True                 
            elif n >= 500 and  remain_team < 3 and battle.can_dynamax == True:
                dyn = True                   
            elif remain_team < 2 and battle.can_dynamax == True:
                dyn = True  
                    
            #if dyn == True:
             #   print(remain_team)

            #dyn = False
            #print(DamCal(best_move, opp))  
            
            return self.create_order(best_move, dynamax = dyn)
        else:
            return self.choose_random_move(battle)


# In[267]:


async def main():
    start = time.time()

    greedy_player = Greedy(battle_format='gen8randombattle')
    greedy_switch_player = Greedy_Switch(battle_format='gen8randombattle')
    #opponent = Greedy(battle_format='gen8randombattle')
    await greedy_switch_player.battle_against(greedy_player,n_battles = 1000)

    print(f'Greedy Switch player won {greedy_switch_player.n_won_battles}; this process took {time.time()-start} seconds')


# In[268]:


if __name__=='__main__':
    #print(type_chart)
    asyncio.get_event_loop().run_until_complete(main())


# In[ ]:




