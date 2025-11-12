# ocaml_mahjong

Zeli Ma, Jiewen Luo, Jin Zhou

Japanese mahjong implemented with ocaml, for FPSE 2025Fall

Finished:

* add mlis in `/lib`
* add ./demo, which is match the requirements of "15% libraries: has a working demo folder for each library to be used in the final submission.", use an initial term of mahjong, see in `/demo/readme.md`
* finish figma demo

Todo:

* Finish design.md, which is for "30% mock use: depicts each usage case clearly and accurately." Descript what we want to implement and add link to the `figma` demo
  * basic mahjong with draw, discard and tsumo(draw the card to win)
  * complex mahjong with more scoring patterns(include  honor card(baopai)), chi(get last players discard to make a sequence), pon(get other players discard to make a triple combination), ron(get other players dicard to win),
  * AI recommend strategies: calculate the value of each card to play, using A* algorithm to search
* Add document to mlis, use english(just translate the english term)
* Plan for "10% plan of implementation: there is a detailed implementation plan that covers all aspects of the project."
  * better to write a timeline including what steps to do
* Some explanation of "15% project scope: the project is not too big or too small, has enough algorithmic complexity, and has room to make a general library.", this might be included in the design.md
* long term plan will be in `plan.md`