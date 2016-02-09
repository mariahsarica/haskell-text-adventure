# Text Adventure Game written in Haskell
## Author: Mariah Molenaer


1. *Theme & Backstory:*
   My game is about a player travelling through a grocery store, gathering items as they
   go along. When the player gets to a certain section in the store, they must defeat the
   boss character.

2. *World & Locations:*
   The setting for my game is a grocery store, there are different sections, and aisles
   that the player can travel through in the store.

3. *Items & Objects:*
   There are different grocery items that the player can pick up as they travel from
   section to section. These items will help the player in their ultimate task of defeating
   the boss character. Before the player can pick up grocery items, they must first take
   a cart.

4. *Goals & Obstacles:*
   The ultimate goal for the player is to defeat the boss character. Major steps the player
   must take are to take as many items as possible as these will help defeat him. Upon
   beginning the game, the player does not know that they will need to fight a character,
   they only find out once they enter the room.

5. *Essential Types & Classes:*
   The type Int will be needed to represent the player's and boss' health. A Matrix will 
   be needed to represent the world, and how the player moves to different locations. I 
   may have to define new classes for the items and the locations. The Bool type will be 
   needed for any if.. then functions I write. The Eq and Ord classes will be needed for 
   equality and comparison testing in any of my functions.

6. *Function Design:*
   I will need a function to *take* items. The input will be an item from the Item class.
   The item will be added to a list of the players inventory. A function for *north, south,
   east, and west* so the player can move to a different locations. The input will be an
   Int relating to the location matrix and the output will be that the player is in a new
   location.
   
