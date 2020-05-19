# python notebook for Make Your Own Neural Network
# code for a 3-layer neural network, and code for learning the MNIST dataset
# this version creates additional training examples by rotating each original by +/- 10 degrees
# (c) Tariq Rashid, 2016
# license is GPLv2

# numpy provides arrays and useful functions for working with them
import numpy
# scipy.special for the sigmoid function expit()
import scipy.special
# scipy.ndimage for rotating image arrays
import scipy.ndimage
import sys

# neural network class definition
class neuralNetwork:
    # initialise the neural network
    def __init__(self, inputnodes, hiddennodes, outputnodes, learningrate, epochs):
        # set number of nodes in each input, hidden, output layer
        self.inodes = inputnodes
        self.hnodes = hiddennodes
        self.onodes = outputnodes
        self.epochs = epochs

        # link weight matrices, wih and who
        # weights inside the arrays are w_i_j, where link is from node i to node j in the next layer
        # w11 w21
        # w12 w22 etc
        self.wih = numpy.random.normal(0.0, pow(self.inodes, -0.5), (self.hnodes, self.inodes))
        self.who = numpy.random.normal(0.0, pow(self.hnodes, -0.5), (self.onodes, self.hnodes))

        # learning rate
        self.lr = learningrate

        # activation function is the sigmoid function
        self.activation_function = lambda x: scipy.special.expit(x)

        pass

    # save neural network weights
    def save(self):
        numpy.save(str(self.lr ) + '_' + str(self.hnodes) + '_' + str(self.epochs) + '_saved_wih.npy', self.wih)
        numpy.save(str(self.lr ) + '_' + str(self.hnodes) + '_' + str(self.epochs) + '_saved_who.npy', self.who)
        pass

    # load neural network weights
    def load(self):
        self.wih = numpy.load(str(self.lr ) + '_' + str(self.hnodes) + '_' + str(self.epochs) + '_saved_wih.npy')
        self.who = numpy.load(str(self.lr ) + '_' + str(self.hnodes) + '_' + str(self.epochs) + '_saved_who.npy')
        pass

    # train the neural network
    def train(self, inputs_list, targets_list):
        # convert inputs list to 2d array
        inputs = numpy.array(inputs_list, ndmin=2).T
        targets = numpy.array(targets_list, ndmin=2).T

        # calculate signals into hidden layer
        hidden_inputs = numpy.dot(self.wih, inputs)
        # calculate the signals emerging from hidden layer
        hidden_outputs = self.activation_function(hidden_inputs)

        # calculate signals into final output layer
        final_inputs = numpy.dot(self.who, hidden_outputs)
        # calculate the signals emerging from final output layer
        final_outputs = self.activation_function(final_inputs)

        # output layer error is the (target - actual)
        output_errors = targets - final_outputs
        # hidden layer error is the output_errors, split by weights, recombined at hidden nodes
        hidden_errors = numpy.dot(self.who.T, output_errors)

        # update the weights for the links between the hidden and output layers
        self.who += self.lr * numpy.dot((output_errors * final_outputs * (1.0 - final_outputs)), numpy.transpose(hidden_outputs))

        # update the weights for the links between the input and hidden layers
        self.wih += self.lr * numpy.dot((hidden_errors * hidden_outputs * (1.0 - hidden_outputs)), numpy.transpose(inputs))
        pass

    # query the neural network
    def query(self, inputs_list):
        # convert inputs list to 2d array
        inputs = numpy.array(inputs_list, ndmin=2).T

        # calculate signals into hidden layer
        hidden_inputs = numpy.dot(self.wih, inputs)
        # calculate the signals emerging from hidden layer
        hidden_outputs = self.activation_function(hidden_inputs)

        # calculate signals into final output layer
        final_inputs = numpy.dot(self.who, hidden_outputs)
        # calculate the signals emerging from final output layer
        final_outputs = self.activation_function(final_inputs)

        return final_outputs


# 
# number of input, hidden and output nodes
input_nodes = 16
hidden_nodes = 200
output_nodes = 16

# learning rate
learning_rate = 0.01

# train the neural network
# epochs is the number of times the training data set is used for training
epochs = 10

if sys.argv[1] != "" : learning_rate = float(sys.argv[1])
if sys.argv[2] != "" : hidden_nodes = int(sys.argv[2])
if sys.argv[3] != "" : epochs = int(sys.argv[3])


