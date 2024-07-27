from flask import Flask, request, jsonify
from flask_cors import CORS
from flask_socketio import SocketIO, emit, join_room, leave_room
import pyodbc
import smtplib
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText

app = Flask(__name__)
CORS(app)
socketio = SocketIO(app, cors_allowed_origins="*")

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
    <img src="assets/logo/coficab.png"><br>
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

@app.route('/api/login', methods=['POST'])
def login():
    data = request.get_json()
    email = data['email']
    password = data['password']
    
    cursor = conn.cursor()
    cursor.execute('SELECT * FROM users WHERE email = ? AND password = ?', (email, password))
    user = cursor.fetchone()
     
    if user:
        response = {
            'email': user.email,
            'ROLE': user.ROLE,  
            'username': user.username
        }
        return jsonify(response), 200
    else:
        return jsonify({'error': 'Invalid email or password'}), 401

@app.route('/api/signup', methods=['POST'])
def signup():
    data = request.get_json()
    email = data.get('email')
    password = data.get('password')
    role = data.get('ROLE')
    username = data.get('username')

    cursor = conn.cursor()
    cursor.execute('SELECT * FROM users WHERE email = ?', (email,))
    existing_user = cursor.fetchone()
    if existing_user:
        cursor.close()
        return jsonify({'error': 'Email already in use'}), 400
    else:
        cursor.execute('INSERT INTO users (email, password, ROLE, username) VALUES (?, ?, ?, ?)', (email, password, role, username))
        conn.commit()
        cursor.close()
        return jsonify({'message': 'User added successfully'}), 201

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
