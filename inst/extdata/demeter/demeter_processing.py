import demeter
import sys

config_file = 'config_files/' + str(sys.argv[1])+'.ini'

# run all time steps
demeter.run_model(config_file=config_file,
                  write_outputs=True)


