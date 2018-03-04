from flask import Flask, request, render_template, jsonify

from game import build_game_state
from agent import predict

from keras.models import load_model
from loss import softmax_cross_entropy_with_logits

app = Flask(__name__)

global model
model = load_model("model.h5", custom_objects={'softmax_cross_entropy_with_logits': softmax_cross_entropy_with_logits})

@app.route('/', methods=['GET'])
def index():
    return render_template("index.html")

@app.route('/get_move', methods=['POST'])
def get_move():
    request_data = request.get_json()

    state = request_data['state']
    player_turn = request_data['player_turn']

    game_state = build_game_state(state, player_turn)

    return jsonify({'move': int(predict(game_state, model))})
