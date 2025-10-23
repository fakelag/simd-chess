## simd-chess

simd-chess is a chess engine with the primary aim of utilising specific hardware instructions to gain massive speed advantages over traditional engines. The main compilation target of the project is avx512 compatible processors such as AMD and Intel. For AMD processors it is recommended to use Zen 5+ hardware for its [native 512-bit datapath](https://www.numberworld.org/blogs/2024_8_7_zen5_avx512_teardown/). The engine uses a [NNUE](https://en.wikipedia.org/wiki/Efficiently_updatable_neural_network) style neural network that is trained from selfplay against itself.

## Feature overview

### Engine

| Feature                | Status | Notes                                                                                            |
| ---------------------- | ------ | ------------------------------------------------------------------------------------------------ |
| UCI                    | ✅     |                                                                                                  |
| PV-Search w/ αβ bounds | ✅     | Incl. iterative deepening, quiescence search and typical pruning techniques like LMR, NMP, etc.. |
| Transposition table    | ✅     | 3-fold repetitions and fifty-move rule are also accounted for in the search                      |
| MVVLVA move ordering   | ✅     |                                                                                                  |
| NNUE evaluation        | ✅     | Supports a dual perspective network in the form of (768 -> H)x2 -> 1                             |
| Search threading       | TODO   |                                                                                                  |

### Toolchain

| Feature                                     | Keybind | Notes                                                                                             |
| ------------------------------------------- | ------- | ------------------------------------------------------------------------------------------------- |
| Interactive UI                              | ✅      | [imgui-rs](https://github.com/imgui-rs/imgui-rs), enabled with gui command `cargo run -r gui`     |
| Selfplay engine for neural network training | ✅      |                                                                                                   |
| Matchmaking system                          | ✅      |                                                                                                   |
| Unit tests                                  | ✅      | Incl. [perft](https://www.chessprogramming.org/Perft) style fuzztests for most important features |

## Optimisations
