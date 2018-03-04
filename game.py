import numpy as np


def build_game_state(state_array, player_turn):
    board = np.array(state_array)
    state_binary = _binary(board, player_turn)
    allowed_actions = _allowed_actions(board)

    return {
        'board': board,
        'player_turn': player_turn,
        'state_binary': state_binary,
        'input_dim': (2, 3, 3),
        'allowed_actions': allowed_actions
    }


def _binary(board, player_turn):
    current_player_position = np.zeros(len(board), dtype=np.int)
    current_player_position[board == player_turn] = 1

    other_position = np.zeros(len(board), dtype=np.int)
    other_position[board == -player_turn] = 1

    return (np.append(current_player_position, other_position))


def _allowed_actions(board):
    return np.where(board == 0)[0].tolist()
