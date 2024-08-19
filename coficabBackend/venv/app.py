from flask import Flask, request, jsonify
from flask_cors import CORS
from flask_socketio import SocketIO, emit, join_room, leave_room
import pyodbc
import smtplib
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
import subprocess
import csv
import io
from werkzeug.security import generate_password_hash, check_password_hash
 
import jwt
from functools import wraps
import secrets
import datetime

app = Flask(__name__)
CORS(app)
  # Generates a random 32-character hexadecimal string
# Generate a secure random key for SECRET_KEY and JWT_SECRET_KEY
secret_key = secrets.token_hex(32)  # 64 characters long
jwt_secret_key = secrets.token_hex(32)  # 64 characters long

# Setup SocketIO
socketio = SocketIO(app, cors_allowed_origins="*")
app.config['SECRET_KEY'] = secret_key
app.config['JWT_SECRET_KEY'] = jwt_secret_key
socketio = SocketIO(app, cors_allowed_origins="*")
 
# Database connection
conn = pyodbc.connect(
    'DRIVER={SQL SERVER};'
    'SERVER=DESKTOP-OQ6BTAR;'
    'DATABASE=COFICABTN;'
    'Trusted_Connection=yes;'
)

# Routes
@app.route('/api/sendemail', methods=['POST'])
def send_email():
    data = request.get_json()
    recipient = data.get('recipient')
    subject = data.get('subject')
    body = data.get('body')
    sender = 'onsyahmadi481@gmail.com'
    token = 'vsjz mitq mkms kpvh'

    msg = MIMEMultipart()
    msg['From'] = sender
    msg['To'] = recipient
    msg['Subject'] = subject
    body_with_signature = f"""
    {body}
    <br><br>
    Corporate Data Science<br>
    COFICAB GROUP<br><br>
    Phone: +(216) 71 156 000 (Ext: 3983)<br>
    Mobile: +(216) 93 084 104<br><br>
    ___________________________________________<br><br>
    Confidentiality Disclaimer<br>
    All information contained in this e-mail, including the attachments is confidential, privileged, and only for the information of the intended recipient. It may not be duplicated, shared, or redistributed without the prior written consent of legal representatives of Coficab Tunisie SA.<br><br>
    *******************************************************************************************<br><br>
    Note: If the reader of this message is not the intended recipient, or an employee or agent responsible for delivering this message to the intended recipient, you are hereby notified that any dissemination, distribution or copying of this communication is strictly prohibited. If you have received this communication in error, please notify us immediately by replying to the message and deleting it from your computer.<br><br>
    Mention légale de Confidentialité<br>
    Les informations contenues dans ce message électronique sont confidentielles, privilégiées et uniquement pour l'information du destinataire prévu. Elles ne peuvent être dupliquées, publiées ou redistribuées sans le consentement écrit préalable des représentants légaux de Coficab Tunisie SA.<br><br>
    *******************************************************************************************<br><br>
    Remarque : Si le lecteur de ce message n'est pas le destinataire prévu, ou un employé ou un agent responsable de la transmission de ce message au destinataire prévu, vous êtes informés que toute diffusion, distribution ou copie de cette communication est strictement interdite. Si vous avez reçu cette communication par erreur, veuillez nous en aviser immédiatement en répondant au message et en le supprimant de votre ordinateur.<br><br>
    <img src="C:/Users/HP/Desktop/coficab/coficabBackend/venv/assets/logo/coficab.png"><br>
    Please consider your environmental responsibility before printing this e-mail.
    """
    msg.attach(MIMEText(body_with_signature, 'html'))

    try:
        server = smtplib.SMTP('smtp.gmail.com', 587)
        server.starttls()
        server.login(sender, token)
        server.sendmail(sender, recipient, msg.as_string())
        server.quit()
        return jsonify({'message': 'Email sent successfully'}), 200
    except Exception as e:
        return jsonify({'error': str(e)}), 500


@app.route('/api/tables', methods=['GET'])
def get_tables():
    cursor = conn.cursor()
    try:
        cursor.execute("SELECT table_name FROM information_schema.tables WHERE table_type = 'BASE TABLE' AND table_schema = 'dbo'")
        tables = cursor.fetchall()
        table_list = [table[0] for table in tables if table[0] not in ('users', 'messages')]
    except Exception as e:
        return jsonify({'error': str(e)}), 500
    finally:
        cursor.close()

    return jsonify(table_list), 200
@app.route('/api/table-data/<string:table>', methods=['GET'])
def get_table_data(table):
    cursor = conn.cursor()
    try:
        cursor.execute(f"SELECT * FROM {table}")
        rows = cursor.fetchall()
        columns = [column[0] for column in cursor.description]
        table_data = [dict(zip(columns, row)) for row in rows]
        return jsonify(table_data), 200
    except Exception as e:
        return jsonify({'error': str(e)}), 500
    finally:
        cursor.close()

@app.route('/api/run-r-script', methods=['POST'])
def run_r_scripts():
    try:
        # Path to Rscript executable
        rscript_path = 'C:/Program Files/R/R-4.3.3/bin/Rscript.exe'  # Replace with the actual path to Rscript on your system
        
        # List of R script paths
        script_paths = [
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/scrapeEchange.R',
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/scrapeAlumLME.R',
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/scrapeCopperLME.R',
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/scrapeoil.R',
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/scrapeTinLME.R'
        ]
        
        results = []
        
        for script_path in script_paths:
            # Execute each R script
            result = subprocess.run([rscript_path, script_path], capture_output=True, text=True)
            
            # Check for errors
            if result.returncode != 0:
                error_message = result.stderr
                print(f"Error executing R script {script_path}: {error_message}")
                results.append({'script': script_path, 'message': 'Failed to run R script', 'error': error_message})
            else:
                results.append({'script': script_path, 'message': 'R script executed successfully', 'output': result.stdout})
        
        return jsonify(results), 200
    except Exception as e:
        print(f"Exception occurred: {str(e)}")
        return jsonify({'message': 'An error occurred', 'error': str(e)}), 500

@app.route('/api/run-rc-script', methods=['POST'])
def run_rc_scripts():
    try:
        # Path to Rscript executable
        rscript_path = 'C:/Program Files/R/R-4.3.3/bin/Rscript.exe'  # Replace with the actual path to Rscript on your system
        
        # List of R script paths
        script_paths = [
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/cleaningCopperLMEoil.R',
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/cleaningexchangeCOMEX.R',
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/cleaningexchanges.R',
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/cleantin.R',
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/dataexplorationSHMET.R',
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/explorationAlum.R'
        ]
        
        results = []
        
        for script_path in script_paths:
            # Execute each R script
            result = subprocess.run([rscript_path, script_path], capture_output=True, text=True)
            
            # Check for errors
            if result.returncode != 0:
                error_message = result.stderr
                print(f"Error executing R script {script_path}: {error_message}")
                results.append({'script': script_path, 'message': 'Failed to run R script', 'error': error_message})
            else:
                results.append({'script': script_path, 'message': 'R script executed successfully', 'output': result.stdout})
        
        return jsonify(results), 200
    except Exception as e:
        print(f"Exception occurred: {str(e)}")
        return jsonify({'message': 'An error occurred', 'error': str(e)}), 500
@app.route('/api/run-rf-script', methods=['POST'])
def run_rf_scripts():
    try:
        # Path to Rscript executable
        rscript_path = 'C:/Program Files/R/R-4.3.3/bin/Rscript.exe'  # Replace with the actual path to Rscript on your system
        
        # List of R script paths
        script_paths = [
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/forcastCopper.R',
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/forcastAlum.R',
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/forcastCopperCOMEX.R',
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/forcastCopperSHMET.R',
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/forcastdataEUR_TND.R',
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/forcastdataTIN.R',
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/forcastEUR_CNY.R',
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/forcastEUR_MAD.R',
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/forcastEUR_MKD.R',
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/forcastEUR_RON.R',
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/forcastEUR_USD.R',
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/forcastRON_RSD.R',
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/forcastUSD_HNL.R',
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/forcastUSD_MKD.R',
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/forcastUSD_MXN.R',
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/forcastEUR_MXN.R',
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/forcastUSD_RON.R',
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/forecastEUR_HNL.R',
            'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/src/forecastEUR_RSD.R' 
        ]
        
        results = []
        
        for script_path in script_paths:
            # Execute each R script
            result = subprocess.run([rscript_path, script_path], capture_output=True, text=True)
            
            # Check for errors
            if result.returncode != 0:
                error_message = result.stderr
                print(f"Error executing R script {script_path}: {error_message}")
                results.append({'script': script_path, 'message': 'Failed to run R script', 'error': error_message})
            else:
                results.append({'script': script_path, 'message': 'R script executed successfully', 'output': result.stdout})
        
        return jsonify(results), 200
    except Exception as e:
        print(f"Exception occurred: {str(e)}")
        return jsonify({'message': 'An error occurred', 'error': str(e)}), 500
@app.route('/api/run-ssis', methods=['POST'])
def run_ssis():
    try:
        # Path to your SSIS package
        ssis_package_path = 'C:/Users/HP/Documents/Visual Studio 2010/Projects/COFICABTN/COFICABTN/package.dtsx'  # Remplacez par le chemin réel de votre package SSIS
        
        # Command to execute the SSIS package
        command = ['dtexec', '/F', ssis_package_path]

        # Execute the command
        result = subprocess.run(command, capture_output=True, text=True)

        # Check for errors
        if result.returncode != 0:
            error_message = result.stderr
            print(f"Error executing SSIS package: {error_message}")
            return jsonify({'message': 'Failed to run SSIS package', 'error': error_message}), 500

        return jsonify({'message': 'SSIS package executed successfully', 'output': result.stdout}), 200
    except Exception as e:
        print(f"Exception occurred: {str(e)}")
        return jsonify({'message': 'An error occurred', 'error': str(e)}), 500

    

from email.mime.image import MIMEImage

@app.route('/api/send_table_csv', methods=['POST'])
def send_table_csv():
    data = request.get_json()
    table_name = data.get('table_name')
    recipient = data.get('recipient')
    subject = data.get('subject', f'Data from {table_name}')
    sender = 'onsyahmadi481@gmail.com'
    token = 'vsjz mitq mkms kpvh'

    cursor = conn.cursor()
    cursor.execute(f'SELECT * FROM {table_name}')
    rows = cursor.fetchall()
    column_names = [desc[0] for desc in cursor.description]
    cursor.close()

    # Create CSV in memory
    csv_file = io.StringIO()
    csv_writer = csv.writer(csv_file)
    csv_writer.writerow(column_names)
    csv_writer.writerows(rows)
    csv_content = csv_file.getvalue()
    csv_file.close()

    # Create email
    msg = MIMEMultipart('related')
    msg['From'] = sender
    msg['To'] = recipient
    msg['Subject'] = subject

    body_with_signature = f"""
    <p>Find attached the data from table <strong>{table_name}</strong>.</p>
    <br>
    <br>
    Corporate Data Science<br>
    COFICAB GROUP<br><br>
    Phone: +(216) 71 156 000 (Ext: 3983)<br>
    Mobile: +(216) 93 084 104<br><br>
    ___________________________________________<br><br>
    Confidentiality Disclaimer<br>
    All information contained in this e-mail, including the attachments is confidential, privileged, and only for the information of the intended recipient. It may not be duplicated, shared, or redistributed without the prior written consent of legal representatives of Coficab Tunisie SA.<br><br>
    *******************************************************************************************<br><br>
    Note: If the reader of this message is not the intended recipient, or an employee or agent responsible for delivering this message to the intended recipient, you are hereby notified that any dissemination, distribution or copying of this communication is strictly prohibited. If you have received this communication in error, please notify us immediately by replying to the message and deleting it from your computer.<br><br>
    Mention légale de Confidentialité<br>
    Les informations contenues dans ce message électronique sont confidentielles, privilégiées et uniquement pour l'information du destinataire prévu. Elles ne peuvent être dupliquées, publiées ou redistribuées sans le consentement écrit préalable des représentants légaux de Coficab Tunisie SA.<br><br>
    *******************************************************************************************<br><br>
    Remarque : Si le lecteur de ce message n'est pas le destinataire prévu, ou un employé ou un agent responsable de la transmission de ce message au destinataire prévu, vous êtes informés que toute diffusion, distribution ou copie de cette communication est strictement interdite. Si vous avez reçu cette communication par erreur, veuillez nous en aviser immédiatement en répondant au message et en le supprimant de votre ordinateur.<br><br>
    <img src="cid:logo_image"><br>
    Please consider your environmental responsibility before printing this e-mail.
    """
    msg.attach(MIMEText(body_with_signature, 'html'))

    # Attach CSV
    attachment = MIMEText(csv_content, 'csv')
    attachment.add_header('Content-Disposition', 'attachment', filename=f'{table_name}.csv')
    msg.attach(attachment)

    # Attach Image
    with open('C:/Users/HP/Desktop/coficab/coficabBackend/venv/assets/logo/coficab.png', 'rb') as img_file:
        img = MIMEImage(img_file.read())
        img.add_header('Content-ID', '<logo_image>')
        img.add_header('Content-Disposition', 'inline', filename='logo.png')
        msg.attach(img)

    try:
        server = smtplib.SMTP('smtp.gmail.com', 587)
        server.starttls()
        server.login(sender, token)
        server.sendmail(sender, recipient, msg.as_string())
        server.quit()
        return jsonify({'message': 'Email sent successfully with the CSV attachment'}), 200
    except Exception as e:
        return jsonify({'error': str(e)}), 500



 



@app.route('/api/users', methods=['GET'])
def get_all_users():
    cursor = conn.cursor()
    cursor.execute('SELECT * FROM users')
    users = cursor.fetchall()
    user_list = []
    for user in users:
        user_list.append({
            'id': user.id,
            'email': user.email,
            'ROLE': user.ROLE,
            'username': user.username
        })
    return jsonify(user_list), 200

@app.route('/api/users/<int:id>', methods=['PUT'])
def update_user(id):
    data = request.get_json()
    email = data.get('email')
    password = data.get('password')
    role = data.get('ROLE')
    username = data.get('username')

    cursor = conn.cursor()
    cursor.execute('UPDATE users SET email = ?, password = ?, ROLE = ?, username = ? WHERE id = ?', 
                   (email, password, role, username, id))
    conn.commit()
    cursor.close()

    return jsonify({'message': 'User updated successfully'}), 200

@app.route('/api/users/<int:id>', methods=['DELETE'])
def delete_user(id):
    cursor = conn.cursor()
    cursor.execute('DELETE FROM users WHERE id = ?', (id,))
    conn.commit()
    cursor.close()
    return jsonify({'message': 'User deleted successfully'}), 200    
 
@app.route('/api/messages', methods=['GET'])
def get_messages():
    sender_email = request.args.get('sender_email')
    recipient_email = request.args.get('recipient_email')

    cursor = conn.cursor()
    try:
        cursor.execute(
            'SELECT sender_email, recipient_email, message, timestamp FROM messages WHERE (sender_email = ? AND recipient_email = ?) OR (sender_email = ? AND recipient_email = ?) ORDER BY timestamp ASC',
            (sender_email, recipient_email, recipient_email, sender_email)
        )
        messages = cursor.fetchall()
        message_list = []
        for message in messages:
            message_list.append({
                'sender_email': message[0],
                'recipient_email': message[1],
                'message': message[2],
                'timestamp': message[3]
            })
    except Exception as e:
        return jsonify({'error': str(e)}), 500
    finally:
        cursor.close()
    
    return jsonify(message_list), 200
@app.route('/api/send_message', methods=['POST'])
def send_message():
    data = request.get_json()
    sender_email = data.get('sender_email')
    recipient_email = data.get('recipient_email')
    message = data.get('message')
    
    cursor = conn.cursor()
    try:
        cursor.execute('INSERT INTO messages (sender_email, recipient_email, message) VALUES (?, ?, ?)', 
                       (sender_email, recipient_email, message))
        conn.commit()
    except Exception as e:
        conn.rollback()
        return jsonify({'error': str(e)}), 500
    finally:
        cursor.close()
    
    return jsonify({'message': 'Message sent successfully'}), 201


socketio = SocketIO(app, cors_allowed_origins="*")
@socketio.on('send_message')
def handle_send_message_event(data):
    sender_email = data['sender_email']
    recipient_email = data['recipient_email']
    message = data['message']
    room = recipient_email

    cursor = conn.cursor()
    cursor.execute('INSERT INTO messages (sender_email, recipient_email, message) VALUES (?, ?, ?)',(sender_email, recipient_email, message))
    conn.commit()
    cursor.close()

    emit('receive_message', data, room=recipient_email)
 
 

# Decorator for token-required routes
def token_required(f):
    @wraps(f)
    def decorated(*args, **kwargs):
        token = request.headers.get('Authorization')
        print(f"Token received: {token}")
        if not token:
            return jsonify({'message': 'Token is missing!'}), 403
        try:
            token = token.split(' ')[1]
            print(f"Token after split: {token}")
            data = jwt.decode(token, app.config['JWT_SECRET_KEY'], algorithms=["HS256"])
            current_user = data['identity']
        except Exception as e:
            print(f"Token error: {e}")
            return jsonify({'message': 'Token is invalid!'}), 403
        return f(current_user, *args, **kwargs)
    return decorated


# Signup route
@app.route('/api/signup', methods=['POST'])
def signup():
    data = request.get_json()
    email = data['email']
    password = data['password']
    username = data['username']
    role = data['ROLE']

    hashed_password = generate_password_hash(password, method='pbkdf2:sha256')
    
    cursor = conn.cursor()
    cursor.execute(
        'INSERT INTO users (email, password, ROLE, username) VALUES (?, ?, ?, ?)', 
        (email, hashed_password, role, username)
    )
    conn.commit()
    return jsonify({'message': 'User registered successfully!'}), 201

# Login route
@app.route('/api/login', methods=['POST'])
def login():
    data = request.get_json()
    email = data['email']
    password = data['password']
    
    cursor = conn.cursor()
    cursor.execute('SELECT * FROM users WHERE email = ?', (email,))
    user = cursor.fetchone()
    
    if user and check_password_hash(user.password, password):
        token = jwt.encode({
            'identity': email,
            'exp': datetime.datetime.utcnow() + datetime.timedelta(hours=1)
        }, app.config['JWT_SECRET_KEY'], algorithm='HS256')
        
        response = {
            'token': token,
            'email': user.email,
            'ROLE': user.ROLE,
            'username': user.username
        }
        return jsonify(response), 200
    else:
        return jsonify({'message': 'Invalid credentials'}), 401

@app.route('/api/logout', methods=['POST'])
@token_required
def logout(current_user):
    return jsonify({'message': 'Logged out successfully!'}), 200

# Example of a protected route
@app.route('/api/protected', methods=['GET'])
@token_required
def protected_route(current_user):
    return jsonify({'message': f'Welcome {current_user}! This is a protected route.'}), 200

 

@socketio.on('join_room')
def handle_join_room(data):
    email = data['email']
    join_room(email)
    emit('join_room_announcement', f'{email} has joined the room.', room=email)

@socketio.on('leave_room')
def handle_leave_room(data):
    email = data['email']
    leave_room(email)
    emit('leave_room_announcement', f'{email} has left the room.', room=email)

if __name__ == '__main__':
    socketio.run(app, host='0.0.0.0', port=5000)