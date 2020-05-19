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


# create instance of neural network
n = neuralNetwork(input_nodes,hidden_nodes,output_nodes, learning_rate, epochs)

# load the mnist training data CSV file into a list
training_data_file = open("horse_runner_prices_train.csv", 'r')
training_data_list = training_data_file.readlines()
training_data_file.close()


for e in range(epochs):
    # go through all records in the training data set
    cnt = 0
    for record in training_data_list:
        # split the record by the ',' commas
        all_values = record.split(',')
        # scale and shift the inputs
        #inputs = numpy.asfarray(all_values[1:])
        inputs = (numpy.asfarray(all_values[1:]) * 0.99) + 0.01
        # create the target output values (all 0.01, except the desired label which is 0.99)

        targets = numpy.zeros(output_nodes) + 0.01
        # all_values[0] is the target label for this record
        targets[int(float(all_values[0]))] = 0.99
        n.train(inputs, targets)

        cnt = cnt +1
        #print(cnt)
        #if cnt > 10000 : break
        pass
    pass

# load the mnist test data CSV file into a list
test_data_file = open("horse_runner_prices_sample.csv", 'r')
test_data_list = test_data_file.readlines()
test_data_file.close()

# test the neural network
# scorecard for how well the network performs, initially empty
scorecard = []
cnt = 0

# go through all the records in the test data set
for record in test_data_list:
    #split the record by the ',' commas
    #print(cnt)
    all_values = record.split(',')
    # correct answer is first value
    correct_label = int(float(all_values[0]))
    # scale and shift the inputs
    #inputs = (numpy.asfarray(all_values[1:]) / 255.0 * 0.99) + 0.01
    inputs = (numpy.asfarray(all_values[1:]) * 0.99) + 0.01
    # query the network
    outputs = n.query(inputs)

    # the index of the highest value corresponds to the label
    label = numpy.argmax(outputs)
    # append correct or incorrect to list
    if (label == correct_label):
        # network's answer matches correct answer, add 1 to scorecard
        scorecard.append(1)
    else:
        # network's answer doesn't match correct answer, add 0 to scorecard
        scorecard.append(0)
        pass

    cnt = cnt +1
    #if cnt > 100 : break
    pass

# calculate the performance score, the fraction of correct answers
scorecard_array = numpy.asarray(scorecard)
print ("performance = ", scorecard_array.sum() / scorecard_array.size, "lr=", str(learning_rate), "hn=", str(hidden_nodes), "ep=", str(epochs))

n.save()

