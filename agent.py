import numpy as np


def predict(game_state, model):
    input_to_model = np.array([_convert_to_model_input(game_state)])

    preds = model.predict(input_to_model)
    value_array = preds[0]
    logits_array = preds[1]
    value = value_array[0]

    logits = logits_array[0]

    allowed_actions = game_state['allowed_actions']

    mask = np.ones(logits.shape, dtype=bool)
    mask[allowed_actions] = False
    logits[mask] = -100

    #SOFTMAX
    odds = np.exp(logits)
    probs = odds / np.sum(odds)

    return np.argmax(probs)


def _convert_to_model_input(game_state):
    return (np.reshape(game_state['state_binary'], game_state['input_dim']))
